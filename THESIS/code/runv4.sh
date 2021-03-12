#!/bin/bash

for i in {1..100}
do
  echo 'R CMD BATCH -$i v4_cs1_1_1.R'
  R CMD BATCH -$i v4_cs1_1_1.R

  echo 'R CMD BATCH -$i v4_cs1_2_1.R'
  R CMD BATCH -$i v4_cs1_2_1.R

  echo 'R CMD BATCH -$i v4_cs2_1_1.R'
  R CMD BATCH -$i v4_cs2_1_1.R

  echo 'R CMD BATCH -$i v4_cs2_2_1.R'
  R CMD BATCH -$i v4_cs2_2_1.R

  echo 'R CMD BATCH -$i v4_cs3_1_1.R'
  R CMD BATCH -$i v4_cs3_1_1.R

  echo 'R CMD BATCH -$i v4_cs3_2_1.R'
  R CMD BATCH -$i v4_cs3_2_1.R

  echo 'R CMD BATCH -$i v4_cs1_1_2.R'
  R CMD BATCH -$i v4_cs1_1_2.R

  echo 'R CMD BATCH -$i v4_cs1_2_2.R'
  R CMD BATCH -$i v4_cs1_2_2.R

  echo 'R CMD BATCH -$i v4_cs2_1_2.R'
  R CMD BATCH -$i v4_cs2_1_2.R

  echo 'R CMD BATCH -$i v4_cs2_2_2.R'
  R CMD BATCH -$i v4_cs2_2_2.R

  echo 'R CMD BATCH -$i v4_cs3_1_2.R'
  R CMD BATCH -$i v4_cs3_1_2.R

  echo 'R CMD BATCH -$i v4_cs3_2_2.R'
  R CMD BATCH -$i v4_cs3_2_2.R

  echo 'R CMD BATCH -$i v4_cs1_1_3.R'
  R CMD BATCH -$i v4_cs1_1_3.R

  echo 'R CMD BATCH -$i v4_cs1_2_3.R'
  R CMD BATCH -$i v4_cs1_2_3.R

  echo 'R CMD BATCH -$i v4_cs2_1_3.R'
  R CMD BATCH -$i v4_cs2_1_3.R

  echo 'R CMD BATCH -$i v4_cs2_2_3.R'
  R CMD BATCH -$i v4_cs2_2_3.R

  echo 'R CMD BATCH -$i v4_cs3_1_3.R'
  R CMD BATCH -$i v4_cs3_1_3.R

  echo 'R CMD BATCH -$i v4_cs3_2_3.R'
  R CMD BATCH -$i v4_cs3_2_3.R
done
