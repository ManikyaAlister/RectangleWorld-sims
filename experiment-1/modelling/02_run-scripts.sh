# This script runs seperate parallel jobs for each condition. For each value 
# in the loop, it splits off another parallel run of 03_fit-model.R where the 
# condId corresponds to ii in the loop. 
#
# When running from terminal, needs to be run from the correct working directory. E.g.:
# cd Documents/Projects/RectangleWorld/RectangleWorld-sims/

# If you try to run all 64 at once everything slows down HEAPS on 16GB of ram. ~ 20 is okay. 
for ii in $(seq 40 64)
  do
  Rscript experiment-1/modelling/03.2_fit-model-flat.R $ii &
  done
# wait %%
# # run ~20 in a parallel then run the next batch once first batch is finished.
# for ii in $(seq 56 64)
#   do
#   Rscript experiment-1/modelling/03_fit-model.R $ii &
#   done