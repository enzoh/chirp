FROM ubuntu:xenial

ENV DEBIAN_FRONTEND noninteractive
ENV GOROOT /usr/local/go
ENV GOPATH /root/go
ENV PATH /root/.local/bin:${GOROOT}/bin:${GOPATH}/bin:${PATH}

RUN apt-get update
RUN apt-get install -y autoconf curl git libtool unzip wget

WORKDIR /tmp

### Install Go
RUN wget https://storage.googleapis.com/golang/go1.9.linux-amd64.tar.gz
RUN tar xf go1.9.linux-amd64.tar.gz
RUN mv go /usr/local

### Install Go libraries
RUN mkdir /root/go
RUN go get github.com/enzoh/go-logging
RUN go get github.com/hashicorp/golang-lru
RUN wget https://s3-us-west-2.amazonaws.com/gx-deps/gx.zip
RUN unzip gx.zip -d ${GOPATH}/src

### Install Stack
RUN mkdir -p /root/.local/bin
RUN curl -sSL https://get.haskellstack.org | sh

### Install Revolver
RUN git clone https://github.com/dfinity/hs-revolver
WORKDIR /tmp/hs-revolver
RUN git checkout 053cae4e4bd68c1a31f1d2074e5b5685c337d6e6
RUN make install
RUN stack setup
RUN stack install

### Create workspace
RUN mkdir /workspace
WORKDIR /workspace
