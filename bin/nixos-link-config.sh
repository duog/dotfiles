#!/usr/bin/env bash

machine=${1:?"Specify a machine. e.g. ${0} tyrion "}

if [[ ! -d ${XDG_CONFIG_HOME}/nixos/${machine} ]]; then
    echo "machine '${XDG_CONFIG_HOME}/nixos/${machine}' does not exist"
    exit
fi

cd /etc/nixos

sudo ln -s "${XDG_CONFIG_HOME}/nixos/configuration.nix" 
sudo ln -s "${XDG_CONFIG_HOME}/nixos/${machine}" this-machine

