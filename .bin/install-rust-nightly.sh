#!/bin/sh

wget https://static.rust-lang.org/dist/rustc-nightly-src.tar.gz
wget https://static.rust-lang.org/dist/rustc-nightly-src.tar.gz.asc
gpg --verify rustc-nightly-src.tar.gz{.asc*,}

if [ $status -ne 0 ]; then
    echo "Can't verify rustc-nightly-src" >&2
    return
fi

wget https://static.rust-lang.org/dist/rust-nightly-x86_64-unknown-linux-gnu.tar.gz
wget https://static.rust-lang.org/dist/rust-nightly-x86_64-unknown-linux-gnu.tar.gz.asc
gpg --verify rust-nightly-x86_64-unknown-linux-gnu.tar.gz{.asc*,}

if [ $status -ne 0 ]; then
    echo "Can't verify rust-nightly-x86_64-unknown-linux-gnu" >&2
    return
fi

rm -rf rust
rm -rf rust-nightly-src
rm -rf rust-nightly-x86_64-unknown-linux-gnu

tar -xvzf rustc-nightly-src.tar.gz
mv rustc-nightly-src rust-nightly-src
rm rustc-nightly-src.tar.gz

tar -xvzf rust-nightly-x86_64-unknown-linux-gnu.tar.gz
rm rust-nightly-x86_64-unknown-linux-gnu.tar.gz
./rust-nightly-x86_64-unknown-linux-gnu/install.sh --prefix=~/rust
