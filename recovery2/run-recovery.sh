#!/bin/bash

# Define the values for alpha and clue
alphas=(1 0 -1)
clues=(1 2 3 4)

# Loop through each combination of alpha and clue
for alpha in "${alphas[@]}"; do
  for clue in "${clues[@]}"; do
    # Execute the R script with the current alpha and clue values
    Rscript Recovery2/recovery.R $alpha $clue &
  done
done
