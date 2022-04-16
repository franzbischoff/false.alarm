# Before proceeding, it helps to to review how parsnip categorizes models:

# The model type is related to the structural aspect of the model. For example, the model type linear_reg
#  represents linear models (slopes and intercepts) that model a numeric outcome. Other model types in the
#  package are nearest_neighbor, decision_tree, and so on.

# Within a model type is the mode, related to the modeling goal. Currently the two modes in the package are
#  regression and classification. Some models have methods for both models (e.g. nearest neighbors) while
#  others have only a single mode (e.g. logistic regression).

# The computation engine is a combination of the estimation method and the implementation. For example,
#  for linear regression, one engine is "lm" which uses ordinary least squares analysis via the lm() function.
#  Another engine is "stan" which uses the Stan infrastructure to estimate parameters using Bayes rule.

# When adding a model into parsnip, the user has to specify which modes and engines are used. The package
#  also enables users to add a new mode or engine to an existing model.

# https://tidymodels.github.io/model-implementation-principles/function-interfaces.html
# Top-level model function should be a generic with methods for data frames, formulas, and possibly recipes.
#  These methods error trap bad arguments, format/encode the data, them pass it along to the lower-level computational code.

# Do not require users to create dummy variables from their categorical predictors. Provide a formula and/or recipe
#  interface to your model to do this (see the next item) and other methods should error trap if qualitative data should not be subsequently used.
