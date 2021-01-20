#!/bin/bash

for i in {1..5}
do
  echo 'R CMD BATCH -$i model22-hybrid1.R'
  R CMD BATCH -$i model22-hybrid1.R
  echo 'R CMD BATCH -$i model22-hybrid2.R'
  R CMD BATCH -$i model22-hybrid2.R

  echo 'R CMD BATCH -$i model36-hybrid1.R'
  R CMD BATCH -$i model36-hybrid1.R
  echo 'R CMD BATCH -$i model36-hybrid2.R'
  R CMD BATCH -$i model36-hybrid2.R

  echo 'R CMD BATCH -$i model38-hybrid1.R'
  R CMD BATCH -$i model38-hybrid1.R
  echo 'R CMD BATCH -$i model38-hybrid2.R'
  R CMD BATCH -$i model38-hybrid2.R

  echo 'R CMD BATCH -$i model40-hybrid1.R'
  R CMD BATCH -$i model40-hybrid1.R
  echo 'R CMD BATCH -$i model40-hybrid2.R'
  R CMD BATCH -$i model40-hybrid2.R
done
