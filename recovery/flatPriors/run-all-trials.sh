#!/bin/bash
cd 
cd Documents/Projects/RectangleWorld/RectangleWorld-sims/recovery/flatPriors/

for i in -1 0 1
do
  Rscript allTrials.R $i &
done