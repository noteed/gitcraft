#! /usr/bin/env bash

# This script creates a Git repository and compares its state (using
# git-state.sh) to a similar in-memory repository simulated by Gitcraft.hs.

set -e

export TMP_DIR=/tmp/gitcraft/repository

export GIT_DIR=/tmp/gitcraft/repository/.git
export GIT_AUTHOR_DATE="1970-01-01T00:00:00"
export GIT_COMMITTER_DATE="1970-01-01T00:00:00"
export TZ="UTC"

rm -rf ${TMP_DIR}
mkdir ${TMP_DIR}

git init > /dev/null
git config color.ui false

echo Empty repository...
./git-state.sh > temp-repository
./gitcraft-state.sh 0 >  crft-repository

diff -u temp-repository crft-repository
rm temp-repository crft-repository

echo Initial commit...
git commit -m'Initial commit.' --allow-empty > /dev/null

./git-state.sh > temp-repository
./gitcraft-state.sh 1 >  crft-repository

diff -u temp-repository crft-repository
rm temp-repository crft-repository

echo Add LICENSE, build file...
git commit -m'Add LICENSE, build file.' --allow-empty > /dev/null

./git-state.sh > temp-repository
./gitcraft-state.sh 2 >  crft-repository

diff -u temp-repository crft-repository
rm temp-repository crft-repository

echo SUCCESS.
