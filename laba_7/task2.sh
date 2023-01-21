#!/bin/bash

# path to file = $1

SUMM=0

if [[ ! -z $1 ]]
then
  for file in $(find $1 -name "*.h" && find $1 -name "*.c")
  do
    AMOUNT=$(cat $file | grep '\S' | wc -l)
    let SUMM=$SUMM+$AMOUNT
    echo "amount of no-empty strings in file $file: $AMOUNT"
  done
  echo "amount of no-empty strings in all files *.h and *.c: $SUMM"
else
  echo "no file path"
fi
