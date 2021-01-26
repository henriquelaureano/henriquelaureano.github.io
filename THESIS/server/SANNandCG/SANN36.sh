#!/bin/bash

for i in {1..2}
do
  echo 'R CMD BATCH -$i SANN36.R'
  R CMD BATCH -$i SANN36.R
done
