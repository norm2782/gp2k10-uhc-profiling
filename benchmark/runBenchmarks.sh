#!/bin/bash

RESDIR="results"

#echo "Cleaning up for $1"
#rm -rf "$RESDIR"

echo "Creating results directory"
mkdir "$RESDIR"

echo "Running benchmark with O0"
benchmark -o "$RESDIR/$1-O0" -f -O0

echo "Running benchmark with O1"
benchmark -o "$RESDIR/$1-O1" -f -O1

echo "Running benchmark with O2"
benchmark -o "$RESDIR/$1-O2" -f -O2

echo "Running benchmark with O3"
benchmark -o "$RESDIR/$1-O3" -f -O3

