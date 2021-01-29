#!/bin/bash

for i in {1..2}
do
  echo 'R CMD BATCH -$i SANN22.R'
  R CMD BATCH -$i SANN22.R
  echo 'R CMD BATCH -$i SANN36.R'
  R CMD BATCH -$i SANN36.R
  echo 'R CMD BATCH -$i SANN38.R'
  R CMD BATCH -$i SANN38.R
  echo 'R CMD BATCH -$i SANN40.R'
  R CMD BATCH -$i SANN40.R
done
