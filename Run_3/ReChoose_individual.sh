#!/bin/bash
sed -i -e "s/\(gen = \).*/\1$1;/" ReControl.m
sed -i -e "s/\(ind = \).*/\1$2;/" ReControl.m


#sed -i -e "s/\(cycle = \).*/\1$1;/" Control.m
#sed -i -e "s/\(set_type = \).*/\1'$2';/" Control.m
#sed -i -e "s/\(number_set = \).*/\1$3;/" Control.m
# change the each line to the choose the individual
