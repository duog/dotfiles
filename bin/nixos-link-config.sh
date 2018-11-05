#!/usr/bin/env bash

machine=${1:?"Specify a machine. e.g. ${0} tyrion "}

configdir="${XDG_CONFIG_HOME:-"$HOME/.config"}/nixos"

machinedir="$configdir/$machine"
if [[ ! -d "$machinedir" ]]; then
    echo "machine '$machinedir' does not exist"
    exit
fi

cd /etc/nixos

sudo ln -fs "$configdir/*.nix" 
sudo ln -fs "$machinedir" this-machine

