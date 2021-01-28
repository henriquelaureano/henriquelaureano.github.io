#!/bin/bash

for i in {1..2}
do
  echo 'R CMD BATCH -$i cg22.R'
  R CMD BATCH -$i cg22.R
  echo 'R CMD BATCH -$i cg36.R'
  R CMD BATCH -$i cg36.R
  echo 'R CMD BATCH -$i cg38.R'
  R CMD BATCH -$i cg38.R
  echo 'R CMD BATCH -$i cg40.R'
  R CMD BATCH -$i cg40.R
done
