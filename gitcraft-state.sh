#! /usr/bin/env bash

nix-shell --pure --run "runghc Gitcraft.hs state $1"
