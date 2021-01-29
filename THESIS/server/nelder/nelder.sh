#!/bin/bash

for i in {1..2}
do
  echo 'R CMD BATCH -$i nelder22.R'
  R CMD BATCH -$i nelder22.R
  echo 'R CMD BATCH -$i nelder36.R'
  R CMD BATCH -$i nelder36.R
  echo 'R CMD BATCH -$i nelder38.R'
  R CMD BATCH -$i nelder38.R
  echo 'R CMD BATCH -$i nelder40.R'
  R CMD BATCH -$i nelder40.R
done
