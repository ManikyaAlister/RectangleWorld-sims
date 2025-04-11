# Code and Data for the paper "When a helpful bias is unhelpful: Limitations in reasoning from random and misleading evidence"

Note that `datafiles` does not contain the experiment data (see overview), it contains model output. 

Note also that throughout the code and data, we make reference to Helpful, Random, Misleading, and Uninformative conditions. Here, "Misleading" refers to Misleading Naive in the paper, and "Uninformative" to misleading aware. These conditions are also often abbreviated to the first letter of the condition (e.g., "M" for misleading) and either "S" for  cover *story* or "N" for *no* cover story. So "MS" Would be the *M*isleading cover *S*tory condition. 

## Requirements

R version 4.2.2

## Overview
- `01.1_generate-data-files` and `01.2_generate-recursive-data-files` generate the probability distributions for points and hypotheses in a hypothesis space. The data files (saved in `datafiles`) made from this script are essential for all of the analyses that use the computational modelling. They (especially the recursive script) take a little while to run, though. 
- `experiment-<>` directories: contain all of the data and analysis code for analyzing each of the individual experiments. `experiment-1-rerun` is because the first version of experiment 1 ran with a mistake in the instructions, so we re-ran the experiment to be certain the mistake did not affect the results. This file only contains data, though. All of the results reported in the paper pertaining to E1 and all of the modelling/analyses in the standard `experiment-1/modelling` folder use the data from the reran experiment, not the original E1 data with the mistake.
- `experiment-comparison`: contains all of the analyses that make comparisons across experiments. 
- `experiment-scenarios`: contains data pertaining to the locations of the true rectangles and clues used in the experiment. 
- `expeiment-simulations`: contains files relevant to conducting model simulations of the experiment target blocks.
- `functions`: contains various functions used in the analyses. 
- `model-demonstrations`: contains Rmarkdown/Quarto docs designed to provide an understanding of how the pedagogical sampling assumptions model works.
- `recovery`: contains files relevant to a model recovery that tested whether data generated from the model could be accurately recovered by the model.  I've set the analysis up to run on a remote cloud computing server because it is a pretty computationally analysis, so this allows me to parallelise it greatly. If you want to run it locally, the scripts will need to be adapted. 
