#!/usr/bin/env bash

set -eu

apt-get update -q
apt-get update
apt-get install -y wget

if ! [ -x "$(command -v stack)" ]
then
  curl -sSL https://get.haskellstack.org/ | sh
fi

PG_VERSION=10

export DEBIAN_FRONTEND=noninteractive

PG_REPO_APT_SOURCE=/etc/apt/sources.list.d/pgdg.list
if [ ! -f "$PG_REPO_APT_SOURCE" ]
then
  # Add PG apt repo:
  echo "deb http://apt.postgresql.org/pub/repos/apt/ bionic-pgdg main" > "$PG_REPO_APT_SOURCE"

  # Add PGDG repo key:
  wget --quiet -O - https://apt.postgresql.org/pub/repos/apt/ACCC4CF8.asc | apt-key add -
fi

# Update package list and upgrade all packages
apt-get update
apt-get -y upgrade
apt-get -y install libncurses5-dev libncursesw5-dev
# ln -s /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so
apt-get -y install "postgresql-$PG_VERSION" "postgresql-contrib-$PG_VERSION" "postgresql-server-dev-$PG_VERSION"

su - vagrant

echo "export PATH=$PATH:/usr/lib/postgresql/$PG_VERSION/bin/" >> $HOME/.bashrc

if ! [ -x "$(command -v ghc)" ]
then
  export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
  curl https://get-ghcup.haskell.org -sSf | sh
  . "$HOME/.ghcup/env"
  echo '. $HOME/.ghcup/env' >> $HOME/.bashrc # or similar
  yes | ghcup install 8.6.5
fi
