-module(eetcd_election).
-include("eetcd.hrl").

-export([observe/3, observe_stream/2]).

-export_type([observe_ctx/0]).

-type observe_ctx() :: #{
    leader := map() | election_no_leader,
    http2_pid := pid(),
    monitor_ref := reference(),
    stream_ref := gun:stream_ref(),
    pb_module := module()
}.

%%% @doc Observe streams election proclamations in-order as made by the election's elected leaders.
%%% Timeout is an integer greater than zero which specifies how many milliseconds to wait for a leaders,
%%% or the atom infinity to wait indefinitely. If no leader is received within the specified time,
%%% the function call return 'election_no_leader'. and will streams election proclamations by order messages.
-spec observe(etcd_name(), binary(), timeout()) -> {ok, observe_ctx()}|{error, eetcd_error()}.
observe(EtcdName, Name, Timeout) ->
    Request = #{name => Name},
    {ok, Gun, StreamRef, PbModule} = eetcd_election_gen:observe(EtcdName),
    MRef = erlang:monitor(process, Gun),
    eetcd_stream:data(Gun, StreamRef, Request, 'Etcd.LeaderRequest', fin, PbModule),
    case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
        {response, nofin, 200, _Headers} ->
            case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
                {data, nofin, Body} ->
                    {ok, #{kv := KV}, <<>>}
                        = eetcd_grpc:decode(identity, Body, 'Etcd.LeaderResponse', PbModule),
                    {ok,
                        #{
                            http2_pid => Gun,
                            monitor_ref => MRef,
                            stream_ref => StreamRef,
                            pb_module => PbModule,
                            leader => KV
                        }
                    };
                {error, _} = Err1 ->
                    erlang:demonitor(MRef, [flush]),
                    Err1
            end;
        {response, fin, 200, RespHeaders} ->
            erlang:demonitor(MRef, [flush]),
            {error, eetcd_grpc:grpc_status(RespHeaders)};
        {error, timeout} ->
            {ok,
                #{
                    http2_pid => Gun,
                    monitor_ref => MRef,
                    stream_ref => StreamRef,
                    pb_module => PbModule,
                    leader => election_no_leader
                }
            };
        {error, _} = Err2 ->
            erlang:demonitor(MRef, [flush]),
            Err2
    end.

%%% @doc handle observe stream `Etcd.LeaderResponse'.
-spec observe_stream(observe_ctx(), term()) ->
    unknown|{ok, observe_ctx()} | {error, eetcd_error()}.
observe_stream(#{pb_module := PbModule} = OCtx, Msg) ->
    case resp_stream(OCtx, Msg) of
        {ok, Bin} ->
            {ok, #{kv := KV}, <<>>} =
                eetcd_grpc:decode(identity, Bin, 'Etcd.LeaderResponse', PbModule),
            {ok, OCtx#{leader => KV}};
        Other -> Other
    end.

resp_stream(#{stream_ref := Ref, http2_pid := Pid},
    {gun_response, Pid, Ref, nofin, 200, _Headers}) ->
    receive {gun_data, Pid, Ref, nofin, Bin} ->
        receive {gun_trailers, Pid, Ref, [{<<"grpc-status">>, <<"0">>}, {<<"grpc-message">>, <<>>}]} ->
            {ok, Bin};
        {gun_trailers, Pid, Ref, [{<<"grpc-status">>, GrpcStatus}, {<<"grpc-message">>, GrpcMsg}]} ->
            {error, ?GRPC_ERROR(GrpcStatus, GrpcMsg)}
        after 2000 -> unknown
        end
    after 2000 -> unknown
    end;
resp_stream(#{stream_ref := Ref, http2_pid := Pid},
    {gun_data, Pid, Ref, nofin, Bin}) ->
    {ok, Bin};
resp_stream(#{stream_ref := SRef, http2_pid := Pid, monitor_ref := MRef},
    {gun_trailers, Pid, SRef, [{<<"grpc-status">>, GrpcStatus}, {<<"grpc-message">>, GrpcMsg}]}) ->    %% grpc error
    erlang:demonitor(MRef, [flush]),
    gun:cancel(Pid, SRef),
    {error, ?GRPC_ERROR(GrpcStatus, GrpcMsg)};
resp_stream(#{stream_ref := SRef, http2_pid := Pid, monitor_ref := MRef},
    {gun_error, Pid, SRef, Reason}) -> %% stream error
    erlang:demonitor(MRef, [flush]),
    gun:cancel(Pid, SRef),
    {error, {gun_stream_error, Reason}};
resp_stream(#{http2_pid := Pid, stream_ref := SRef, monitor_ref := MRef},
    {gun_error, Pid, Reason}) -> %% gun connection process state error
    erlang:demonitor(MRef, [flush]),
    gun:cancel(Pid, SRef),
    {error, {gun_conn_error, Reason}};
resp_stream(#{http2_pid := Pid, monitor_ref := MRef},
    {'DOWN', MRef, process, Pid, Reason}) -> %% gun connection down
    erlang:demonitor(MRef, [flush]),
    {error, {gun_down, Reason}};
resp_stream(_OCtx, _UnKnow) -> unknown.
