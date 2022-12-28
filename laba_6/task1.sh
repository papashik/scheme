#!/bin/bash
if [ $# -lt 2 ]
then
  echo "необходимое количество аргументов - 2" >> errors.txt
  exit 1
elif ! [ $1 -eq $1 ]
then
  echo "интервал не является числом" >> errors.txt
  exit 1
elif ! [ -f $2 ]
then 
  echo "не найден файл с программой" >> errors.txt
  exit 1
else
  while true
  do 
    $2 >> script_output.txt &
    wait -n
    echo "end, new process"
    sleep $1s
    wait -n
  done 
fi
