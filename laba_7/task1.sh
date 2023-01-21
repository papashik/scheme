#!/bin/bash

for ind in {1..100}
do
  if [[ ! -e "errors$ind.txt" && ! -e "output$ind.txt" ]]
  then
    ERRORSFILENAME="errors$ind.txt"
    OUTPUTFILENAME="output$ind.txt"
    break
  fi
done

if [ $# -lt 2 ]
then
  echo "необходимое количество аргументов - 2" >> $ERRORSFILENAME
  exit 1
elif ! [ $1 -eq $1 ]
then
  echo "интервал не является числом" >> $ERRORSFILENAME
  exit 1
elif ! [ -f $2 ]
then 
  echo "не найден файл с программой" >> $ERRORSFILENAME
  exit 1
else
  $2 >> $OUTPUTFILENAME &
  VAR=$!
  while true
  do 
    sleep $1s
    if [[ -z $(ps -p $VAR | grep "Done") ]]
    then
      continue
    fi    
    echo "end, new process"
    $2 >> $OUTPUTFILENAME &
    VAR=$!
  done 
fi
