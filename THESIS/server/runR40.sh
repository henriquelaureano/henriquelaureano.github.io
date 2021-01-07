#!/bin/bash

for i in {1..250}
do
  echo 'R CMD BATCH -$i model40.R'
  R CMD BATCH -$i model40.R
done
