#!/bin/sh

set -e
[ "$#" -eq 1 ] || (echo "release name required" && exit 1)

lein cljsbuild once

mkdir -p rel/$1
cp -r index.html css js out rel/$1/
