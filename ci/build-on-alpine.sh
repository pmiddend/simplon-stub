#!/bin/sh

set -eu

echo "Installing system dependencies"
# ghc dependencies
apk add curl binutils-gold gcc g++ git gmp gmp-static gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl pkgconfig tar xz
# specific haskell dependencies
apk add ncurses-static

# project specific dependencies
# zmq is _weird_: why libzmq-static but not libzmq-dev? And vice versa.
apk add hdf5-static hdf5-dev libzmq-static zeromq-dev
echo "Done installing system dependencies"

echo "Installing ghcup"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_GHC_VERSION=9.10 BOOTSTRAP_HASKELL_INSTALL_NO_STACK=y BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

echo "Done installing ghcup"

. ~/.ghcup/env

echo "Building with cabal"
cabal build --enable-executable-static -fuse-static
echo "Done with cabal"
