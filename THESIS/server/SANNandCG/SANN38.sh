#!/bin/bash

for i in {1..2}
do
  echo 'R CMD BATCH -$i SANN38.R'
  R CMD BATCH -$i SANN38.R
done
