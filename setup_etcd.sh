#!/usr/bin/env bash
ETCD_VER=v3.3.18

# choose either URL
GOOGLE_URL=https://storage.googleapis.com/etcd
GITHUB_URL=https://github.com/etcd-io/etcd/releases/download
DOWNLOAD_URL=${GOOGLE_URL}

rm -f /tmp/etcd-${ETCD_VER}-linux-amd64.tar.gz
rm -rf /tmp/etcd && mkdir -p /tmp/etcd

curl -L ${DOWNLOAD_URL}/${ETCD_VER}/etcd-${ETCD_VER}-linux-amd64.tar.gz -o /tmp/etcd-${ETCD_VER}-linux-amd64.tar.gz
tar xzvf /tmp/etcd-${ETCD_VER}-linux-amd64.tar.gz -C /tmp/etcd --strip-components=1
rm -f /tmp/etcd-${ETCD_VER}-linux-amd64.tar.gz

/tmp/etcd/etcd --version
/tmp/etcd/etcdctl --version

/tmp/etcd/etcd --name infra0 --initial-advertise-peer-urls http://127.0.0.1:2380 \
  --listen-peer-urls http://127.0.0.1:2380 \
  --listen-client-urls http://127.0.0.1:2379 \
  --advertise-client-urls http://127.0.0.1:2379 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580 \
  --initial-cluster-state new &

/tmp/etcd/etcd --name infra1 --initial-advertise-peer-urls http://127.0.0.1:2480 \
  --listen-peer-urls http://127.0.0.1:2480 \
  --listen-client-urls http://127.0.0.1:2479 \
  --advertise-client-urls http://127.0.0.1:2479 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580 \
  --initial-cluster-state new &

/tmp/etcd/etcd --name infra2 --initial-advertise-peer-urls http://127.0.0.1:2580 \
  --listen-peer-urls http://127.0.0.1:2580 \
  --listen-client-urls http://127.0.0.1:2579 \
  --advertise-client-urls http://127.0.0.1:2579 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580 \
  --initial-cluster-state new &