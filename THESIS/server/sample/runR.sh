#!/bin/bash

for i in {1..5}
do
  echo 'R CMD BATCH -$i model22.R'
  R CMD BATCH -$i model22.R
  echo 'R CMD BATCH -$i model36.R'
  R CMD BATCH -$i model36.R
  echo 'R CMD BATCH -$i model38.R'
  R CMD BATCH -$i model38.R
  echo 'R CMD BATCH -$i model40.R'
  R CMD BATCH -$i model40.R
done
