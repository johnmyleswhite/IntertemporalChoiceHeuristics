Heuristic and Classical Models of Intertemporal Choice
======================================================

# Overview

This repo contains the data and analysis pipeline used to generate the results
found in the in-press paper, "Money Earlier or Later? Simple Heuristics Explain
Intertemporal Choices Better than Delay Discounting," by Ericson et al. that
will appear in Psychological Science later this year.

# Dependencies

To run this code, you must use a Unix system and have installed:

* R >= v3.0
* Several R packages:
    * boot
    * ggplot2
    * Hmisc
    * plyr
    * reshape
* GNU Make

# Running Analyses

To run the three analyses from the paper, use the `Makefile` to execute each
analysis:

* `make coefficients`
* `make consistent_patterns`
* `make crossvalidated_comparisons`

The last analysis will take more than an hour to complete on a 2012 laptop.

You can also type `make all` to run all of the analyses in sequence without
having to retrigger each analysis separately.

# High-Level Description of Each Analysis

`make coefficients`: This analysis will estimate the coefficients of the ITCH
model for all of the conditions considered in the paper. It will also produce
estimates of standard errors. There will be graphical output found in the
generated `graphs` directory as well as a table of results printed to the
terminal window.

`make consistent_patterns`: This analysis will estimate the null hypothesis's
assigned probability of seeing coefficients that are as similar across
conditions as the ITCH model's coefficients are found to be experimentally.
Output will be printed to the terminal window.

`make crossvalidated_comparisons`: This analysis will perform out-of-sample
comparisons between all of the models considered in the paper by randomly
splitting the full data set 100 times into two parts: a 75% training sample and
a 25% testing sample. There will be graphical output produced in the `graphs`
directory as well as a table of results printed to the files in the
automatically generated `output` directory. Finally, there will be summary
output printed to the terminal window.

# Code Walkthrough

All of the separate analyses have their own file in `src`. Each of these
files starts with the line:

```
source("lib/deps.R")
```

This line pulls in all of the dependencies for the project, including all of
the library code found in the `lib` directory as well as all of the external
package functionality provided by R packages.

### Loading Data

The library file `lib/load_data.R` defines a single function, `load.data`,
which returns a `data.frame` object as output.

If you call

```
load.data(condition_number)
```

The function will retrieve and preprocess all of the data for the condition
numbered `0-5`. The `0` condition is a dataset pooled from the separate data
for all of the other conditions, which are numbered `1-5` just as in the paper.

### Error Metrics

Although the paper only measures the performance of models in terms of mean
absolute deviations, it is also possible to analyze the models in terms of
RMSE, zero-one loss or held-out log-likelihood. All of these model fit metrics
are defined as functions in `lib/error_metrics.R`. Each metric is a separate
function, which can be called as:

```
err(p, y)
```

where `p` is a vector of predicted probabilities of observing `1` outcomes and
`y` is vector of truly observed `1` and `0` outcomes.

### Model Definitions

All of the models used in the paper are defined in `lib/model_definitions.R`.
This file depends upon the `boot` library for R, which provides the `inv.logit`
function we use to estimate non-linear variants of logistic regression.

Every model is defined in (at least) two parts:

* `*.fit.function`: This function outputs a fitted model given input data and
   a baseline level of irreducible noise.
* `*.test.function`: This function evaluates a fitted model against data given
   input data, a set of fitted model parameters, a baseline level of
   irreducible noise, and an error metric.

The models split into three categories:

* _Analytic models_, like the baseline model, which have closed-form solutions
  for model parameters.
* _GLM models_, which have dedicated functionality in R for fitting, via
  the `glm` function.
* _Non-GLM models_, which are fit using numerical optimization through the
  `optim` function.

The non-classical models are hardest to understand, so let's walk through one
of them:

```
exponential.fit.function <- function(data, epsilon) {
    # a in [0.0001, 500.00]
    # delta in [0.01, 0.99]
    f <- function (theta) {
        exponential.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(1.0, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.0000001, 0.01),
        upper=c(500.00, 0.99),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}
```

This function defines an inner function `f`, which is the log likelihood
function that we want to  maximize. This function is a closure, which means
that it only takes in parameters `theta` as arguments, but it depends upon
another variable, `data`, which stores the full data set. This is required by
optimization algorithms, which assume that their inputs are only functions of
model parameters and not functions of any other explicitly-passed variables.

In this example, the closure function `f` is passed to `optim` along with an
initial starting point of `c(1.0, 0.5)`, which refers to the parameters `a` and
`delta` used in the model. `optim` uses the L-BFGS algorithm for optimizing the
log likelihood within a boxed region of space with lower bounds of
`c(0.0000001, 0.01)` and upper bounds of `c(500.00, 0.99)`. To do maximization
of the log likelihood, we set `control = list(fnscale = -1))`, which tells
`optim` to multiply all function values by `-1`. This is because most
optimization algorithms perform minimization by default, not maximization.

Finally, we raise a warning if the algorithm did not converge. This happens
occasionally for some models in some CV replicates of the data. Then we return
the fitted parameters `par`, which are the values of `a` and `delta` as a
vector. This is the fitted model, `theta`, that can be passed to the
function called `exponential.test.function` to evaluate the model on held-out
data.

### Cross-Validation

To ensure that model comparisons are fair between models with very different
numbers of parameters, we perform full cross-validation in the analysis at
`src/crossvalidated_comparisons.R`. In particular, we perform an analysis
strategy often referred to as randomly resampled cross-validation. This
analysis strategy is defined in `lib/cross_validate.R`, which provides code
for randomly repartitioning the full data set into train and test chunks.

### Analysis Code

Each analysis file should be read on its own, knowing that it has full access
to all of the previously mentioned library functions. There is no implicit
coupling between analysis files.
