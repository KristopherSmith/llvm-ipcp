#!/bin/bash

make
clang -emit-llvm -o test/test.bc -c test/test.c

llc test/test.bc -o test/test.s
g++ -o test/test test/test.s 
./test/test

opt -f -load ../lib/IPCO.so -ipco  test/test.bc > test/test.ipco.bc

#llc test/test.slicm.bc -o test/test.slicm.s
#g++ -o test/test test/test.slicm.s 

#./test/test


