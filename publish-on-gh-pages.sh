#!/usr/bin/env bash
set -x
set -e
CURDIR=$(cd $(dirname "$0") && pwd)
SITE_DIR=$1
GH_REMOTE="git@github.com:snaekobbi/sprints.git"
TMP_DIR=$( mktemp -t "$(basename "$0")" )
rm $TMP_DIR
git clone --branch gh-pages --depth 1 $GH_REMOTE $TMP_DIR
rm -rf $TMP_DIR/schedule
cp -r $SITE_DIR $TMP_DIR/schedule
cd $TMP_DIR
git add .
git commit --amend --no-edit
git push --force $GH_REMOTE gh-pages:gh-pages
cd $CURDIR
rm -rf $TMP_DIR
