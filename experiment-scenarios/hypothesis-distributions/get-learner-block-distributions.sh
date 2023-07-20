#!/bin/bash

# Define the values for block and provider
blocks=(1 3 4 5 6 7)
provider=("helpful" "random" "misleading" "uninformative")

# Loop through each combination of block and provider
for block in "${blocks[@]}"; do
  for provider in "${providers[@]}"; do
    # Execute the R script with the current bock and provider values
    Rscript experiment-scenarios/hypothesis-distributions/get-learner-block-distributions.R $block $provider &
  done
done
