#!/bin/bash

generating_strings()
{
echo '
import random
import sys
if not(sys.argv[1].isdigit()) or not(sys.argv[2].isdigit()):
    print("please, use integers as command line arguments")
    quit()
amount = int(sys.argv[1])
len_stroka = int(sys.argv[2])
stroka = ""
for _ in range(amount):
    for _ in range(len_stroka):
        stroka += chr(random.randint(33,126))
    print(stroka)
    stroka = ""
' > strings_test.py

if [[ -z $1 || -z $2 ]]
then
  echo "not enough arguments"
else
  python strings_test.py $1 $2
fi

rm strings_test.py
}
generating_strings $1 $2
