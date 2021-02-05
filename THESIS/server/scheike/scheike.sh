#!/bin/bash

for i in 2
do
  echo 'R CMD BATCH -$i nelder22.R'
  R CMD BATCH -$i nelder22.R
  echo 'R CMD BATCH -$i nelder36.R'
  R CMD BATCH -$i nelder36.R
  echo 'R CMD BATCH -$i nelder38.R'
  R CMD BATCH -$i nelder38.R
  echo 'R CMD BATCH -$i nelder40.R'
  R CMD BATCH -$i nelder40.R
  echo 'R CMD BATCH -$i bfgs22.R'
  R CMD BATCH -$i bfgs22.R
  echo 'R CMD BATCH -$i bfgs36.R'
  R CMD BATCH -$i bfgs36.R
  echo 'R CMD BATCH -$i bfgs38.R'
  R CMD BATCH -$i bfgs38.R
  echo 'R CMD BATCH -$i bfgs40.R'
  R CMD BATCH -$i bfgs40.R
  echo 'R CMD BATCH -$i cg22.R'
  R CMD BATCH -$i cg22.R
  echo 'R CMD BATCH -$i cg36.R'
  R CMD BATCH -$i cg36.R
  echo 'R CMD BATCH -$i cg38.R'
  R CMD BATCH -$i cg38.R
  echo 'R CMD BATCH -$i cg40.R'
  R CMD BATCH -$i cg40.R
  echo 'R CMD BATCH -$i port22.R'
  R CMD BATCH -$i port22.R
  echo 'R CMD BATCH -$i port36.R'
  R CMD BATCH -$i port36.R
  echo 'R CMD BATCH -$i port38.R'
  R CMD BATCH -$i port38.R
  echo 'R CMD BATCH -$i port40.R'
  R CMD BATCH -$i port40.R
  # echo 'R CMD BATCH -$i sann22.R'
  # R CMD BATCH -$i sann22.R
  # echo 'R CMD BATCH -$i sann36.R'
  # R CMD BATCH -$i sann36.R
  # echo 'R CMD BATCH -$i sann38.R'
  # R CMD BATCH -$i sann38.R
  # echo 'R CMD BATCH -$i sann40.R'
  # R CMD BATCH -$i sann40.R
done
