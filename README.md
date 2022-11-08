
# Stacking Characterizations

This repository is a sandbox to look into parameters affecting model
stacking.

## Structure

The `example_analyses` folder contains a series of scripts that
benchmark model stacking with several combinations of datasets and
meta-learners.

The structure of each sub-folder in `example_analyses`, for a dataset
called `dataset`, is as follows:

-   `dataset/`
    -   `prepare_dataset.R`: A script that prepares data and fits a
        series of preprocessors and models to resamples of a dataset.
        Data splits resulting from this script are saved to
        `dataset_data.RData`. Model fit objects resulting from this
        script are saved to the `candidate_fits/` sub-directory.
    -   `dataset_data.RData`: Data splits resulting from
        `prepare_dataset.R`.
    -   `candidate_fits/`: A folder containing model fits given some
        `preproc`essor and `model` on resamples of the `dataset`. Each
        of the objects are stored as a row of a workflow set, and can be
        row-binded together to form a fitted `workflow_set` object.
        -   `dataset_preproc1_model1.RData`
        -   `dataset_preproc1_model2.RData`
        -   …
    -   `fit_members_dataset.R`: A script that reads in each element of
        `candidate_fits/` and fits *all* of them on the entire training
        set. The needed results from this script can then be dropped in
        to model stacks with fitted meta-learners as “fitted members.”
        Doing this step separately from the usual stacks pipeline allows
        for only fitting each base learner on the entire training set
        only once, rather than for each unique combination of
        preprocessor and model.
    -   `member_fits/`: A folder containing model fits given some
        `preproc`essor and `model` on the training set of the `dataset`.
        -   `dataset_preproc1_model1.RData`
        -   `dataset_preproc1_model2.RData`
        -   …
    -   `stack_scripts/`:
        -   `blend_dataset_preproc1_model1.R`: A script that reads in
            each element of `candidate_fits/`, row-binds them together
            to form a workflow set, generates a data stack using the
            workflow set, fits the `preproc`essor and `model` as a
            meta-learner to the data stack, drops in needed fitted
            members, and then generates some basic metrics with the
            fitted model stack. These metrics are saved as
            `dataset_preproc1_model1.Rdata` under `metrics/`.
        -   `blend_dataset_preproc1_model2.R`
        -   …

The top-level folder `metrics` contains the “output” from each of these
experiments, a five-element list with the dataset name, meta-learner
type, time to fit, test set performance metric, and metric value. The
files are named in the format `dataset_preproc_model.RData`.

The top-level folder `meta_learners` contains the code used to generate
the proposed preprocessors and model specifications.

The naming schemes in these experiments are chosen for straightforward
extensibility:

-   Run all of the data preparation scripts (`.R` files starting with
    `prepare_`)
-   Run all of the workflow set fitting scripts (`.R` files starting
    with `fit_candidates_`)
-   Run all of the member fitting scripts (`.R` files starting with
    `fit_members_`)
-   Run all of the blending + benchmarking scripts (`.R` files starting
    with `blend_`)

## Proposed Meta-learners

| ID                | Recipe                       | Model Spec                              |
|:------------------|:-----------------------------|:----------------------------------------|
| `basic_glmnet`    | Minimal                      | Penalized Linear Regression             |
| `basic_xgb`       | Minimal                      | Boosted Tree                            |
| `normalize_bt`    | Center + Scale               | Bagged Tree                             |
| `normalize_bm`    | Center + Scale               | Bagged Mars                             |
| `normalize_svm`   | Center + Scale               | Support Vector Machine (via RBF)        |
| `normalize_nn`    | Center + Scale               | Multi-layer Perceptron (Neural Network) |
| `pca_bt`          | Principal Component Analysis | Bagged Tree                             |
| `pca_bm`          | Principal Component Analysis | Bagged Mars                             |
| `pca_svm`         | Principal Component Analysis | Support Vector Machine (via RBF)        |
| `pca_nn`          | Principal Component Analysis | Multi-layer Perceptron (Neural Network) |
| `renormalize_svm` | C+S, PCA, C+S                | Support Vector Machine (via RBF)        |
| `renormalize_nn`  | C+S, PCA, C+S                | Multi-layer Perceptron (Neural Network) |

------------------------------------------------------------------------

Many `.R` files have analogous `.Rout` files, if ran in batch.
