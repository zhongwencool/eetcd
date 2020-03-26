## Contribution Workflow

This process uses a pretty standard contribution workflow:

1. Fork the repository
2. Create a branch with your changes
3. Make sure all tests pass
4. Submit a pull request
5. Be patient

## Running Tests

### Setting Up etcd Nodes

`eetcd` tests expect an etcd cluster with 3 members running. You can start a cluster any way
you please but must use the same set of ports:

``` bash
/path/to/etcd/etcd --name infra0 --initial-advertise-peer-urls http://127.0.0.1:2380 \
  --listen-peer-urls http://127.0.0.1:2380 \
  --listen-client-urls http://127.0.0.1:2379 \
  --advertise-client-urls http://127.0.0.1:2379 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580 \
  --initial-cluster-state new &
```


``` bash
/path/to/etcd/etcd --name infra1 --initial-advertise-peer-urls http://127.0.0.1:2480 \
  --listen-peer-urls http://127.0.0.1:2480 \
  --listen-client-urls http://127.0.0.1:2479 \
  --advertise-client-urls http://127.0.0.1:2479 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580 \
  --initial-cluster-state new &
```


``` bash
/path/to/etcd/etcd --name infra2 --initial-advertise-peer-urls http://127.0.0.1:2580 \
  --listen-peer-urls http://127.0.0.1:2580 \
  --listen-client-urls http://127.0.0.1:2579 \
  --advertise-client-urls http://127.0.0.1:2579 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580 \
  --initial-cluster-state new &
```

### Running Tests

To run tests, use

``` bash
rebar3 ct
```

`rebar3 ct` can run a specific suite:

``` bash
rebar3 ct --suite eetcd_kv_SUITE
```


### Running Code Generator

``` bash
rebar3 clean
rebar3 etcd gen
rebar3 compile
```
