#!/bin/bash

O0DIR="$1-O0"
O1DIR="$1-O1"
O2DIR="$1-O2"
O3DIR="$1-O3"
CFLAGS="--dump-core-stages=1 --dump-grin-stages=1"

echo "Cleaning up for $1"
rm -rf "$O0DIR"
rm -rf "$O1DIR"
rm -rf "$O2DIR"
rm -rf "$O3DIR"

echo "Creating target directories for $1"
mkdir "$O0DIR"
mkdir "$O1DIR"
mkdir "$O2DIR"
mkdir "$O3DIR"

echo "Copying $1 to target directories"
cp "$1" "$O0DIR"
cp "$1" "$O1DIR"
cp "$1" "$O2DIR"
cp "$1" "$O3DIR"

echo "Compiling $1 with O0"
uhc -O0 $CFLAGS "$O0DIR/$1"

echo "Compiling $1 with O1"
uhc -O1 $CFLAGS "$O1DIR/$1"

echo "Compiling $1 with O2"
uhc -O2 $CFLAGS "$O2DIR/$1"

echo "Compiling $1 with O3"
uhc -O3 $CFLAGS "$O3DIR/$1"
