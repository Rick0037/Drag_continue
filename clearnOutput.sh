#!/bin/bash

pl=2
folder=21

while [ "$pl" != "$folder" ]
do
	rm -f Run_$pl/Code_Output/*
	pl=$(($pl+1))
done
