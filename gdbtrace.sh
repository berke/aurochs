#!/bin/sh

(echo "b parse_start"
echo "r"
echo "a"
echo
echo
echo
while true ; do
  echo "stepi"
done) | gdb ./ck
