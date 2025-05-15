# simplon-stub-hs

![Build](https://github.com/pmiddend/simplon-stub/actions/workflows/build-with-nix.yaml/badge.svg)

A program mimicking the Dectris' detector's Simplon API, reading a H5 file and outputting it via ZeroMQ.

## Usage

Build it, either with `nix build` or via `cabal` and then run it via:

```
simplon-stub --input-h5-file ~/some-file.nx5 --zmq-bind-address tcp://*:9999
```

It will then listen on port 8080 for HTTP requests, and 9999 for ZMQ connections.
