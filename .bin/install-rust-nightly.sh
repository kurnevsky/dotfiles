#!/bin/bash

VERSION=nightly
TARGET=x86_64-unknown-linux-gnu
BASE_URL=https://static.rust-lang.org/dist/

RUST="rust-$VERSION-$TARGET"
RUST_SRC="rustc-$VERSION-src"
RUST_TAR="$RUST.tar.gz"
RUST_SRC_TAR="$RUST_SRC.tar.gz"
RUST_URL="$BASE_URL$RUST_TAR"
RUST_SRC_URL="$BASE_URL$RUST_SRC_TAR"
RUST_INSTALL_PATH="./$RUST-installed"

mkdir -p ~/rust
cd ~/rust || exit 1;

wget "$RUST_SRC_URL"
wget "$RUST_SRC_URL.asc"
gpg --verify "$RUST_SRC_TAR"{.asc,} || { echo "Can't verify $RUST_SRC" >&2; exit 1; }

wget "$RUST_URL"
wget "$RUST_URL.asc"
gpg --verify "$RUST_TAR"{.asc,} || { echo "Can't verify $RUST" >&2; exit 1; }

rm -rf "./$RUST_SRC"
rm -rf "./$RUST"

tar -xvzf "$RUST_SRC_TAR"
tar -xvzf "$RUST_TAR"

rm "$RUST_SRC_TAR"{.asc,}
rm "$RUST_TAR"{.asc,}

rm -rf "$RUST_INSTALL_PATH"

./"$RUST"/install.sh --prefix="$RUST_INSTALL_PATH"
