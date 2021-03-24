#!/bin/bash
FILES=$1

for f in $FILES
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  touch -d "$(git log -1 --pretty="format:%cI" "$f")" "$f"
done
