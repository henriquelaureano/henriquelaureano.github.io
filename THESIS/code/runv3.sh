#!/bin/bash

for i in {1..100}
do
  echo 'R CMD BATCH -$i v3_cs1_1.R'
  R CMD BATCH -$i v3_cs1_1.R

  echo 'R CMD BATCH -$i v3_cs1_2.R'
  R CMD BATCH -$i v3_cs1_2.R

  echo 'R CMD BATCH -$i v3_cs2_1.R'
  R CMD BATCH -$i v3_cs2_1.R

  echo 'R CMD BATCH -$i v3_cs2_2.R'
  R CMD BATCH -$i v3_cs2_2.R

  echo 'R CMD BATCH -$i v3_cs3_1.R'
  R CMD BATCH -$i v3_cs3_1.R

  echo 'R CMD BATCH -$i v3_cs3_2.R'
  R CMD BATCH -$i v3_cs3_2.R
done
