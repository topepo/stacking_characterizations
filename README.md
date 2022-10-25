# Stacking Characterizations

This repository is a sandbox to look into parameters affecting model stacking.

The `example_analyses` folder contains a series of scripts that benchmark model stacking with several combinations of datasets and meta-learners.

The structure of each sub-folder in `example_analyses`, for a dataset called `dataset`, is as follows:

- `dataset/`
  - `dataset.R`: A script that prepares data and fits a series of preprocessors and models to resamples of a dataset. Data splits resulting from this script are saved to `dataset_data.RData`. Model fit objects resulting from this script are saved to the `base_fits/` sub-directory.
  - `dataset_data.RData`: Data splits resulting from `dataset.R`.
  - `base_fits/`: A folder containing model fits given some `preproc`essor and `model` on resamples of a `dataset`. Each of the objects are stored as a row of a workflow set, and can be row-binded together to form a fitted `workflowset` object.
    - `dataset_preproc1_model1.RData`
    - `dataset_preproc1_model2.RData`
    - ...
  - `stack_dataset.R`: A script that reads in each element of `base_fits/` and fits _all_ of them on the entire training set. The needed results from this script can then be dropped in to model stacks with fitted meta-learners as "fitted members." Doing this step separately from the usual stacks pipeline allows for only fitting each base learner on the entire training set only once, rather than for each unique combination of preprocessor and model.
  - `stack_fits/`: A folder containing model fits given some `preproc`essor and `model` on the training set of the `dataset`.
    - `dataset_preproc1_model1.RData`
    - `dataset_preproc1_model2.RData`
    - ...
  - `stack_scripts/`:
    - `stack_preproc1_model1.R`: A script that reads in each element of `base_fits/`, row-binds them together to form a workflow set, fits the first `preproc`essor and `model` as a meta-learner to the workflow set, drops in needed fitted members, and then generates some basic metrics with the fitted model stack. This metrics are saved as `dataset_metrics.Rdata` under `dataset/`.
    - `stack_preproc1_model2.R`
    - ...
    - `metrics/`: 
      - `stack_preproc1_model1.Rdata`: Metrics on the model stacking process from `stack_preproc1_model1.R`.
      - `stack_preproc1_model2.Rdata`
      - ...
