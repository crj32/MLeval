#' Sonar data
#'
#' The Sonar data consist of 208 data points collected on 60 predictors. 
#' The goal is to predict the two classes M for metal cylinder or R for rock.
#' This data has been obtained from the 'mlbench' package. Response variable
#' is in the Class column.
#'
#' @format A data frame with 208 rows as points and 61 variables
"Sonar"

#' Random forest fitted object from Caret on Sonar data
#'
#' Caret was run using 10 fold cross validation on the Sonar data
#' with random forest used to predict the response variable. 
#'
#' @format A Caret train object
"fit"

#' Random forest fitted object from Caret on Sonar data
#'
#' Caret was run using 10 fold repeated cross validation on the Sonar data
#' with random forest used to predict the response variable. 
#'
#' @format A Caret train object
"fit1"

#' Gradient boosted machines fitted object from Caret on Sonar data
#'
#' Caret was run using 10 fold repeated cross validation on Sonar data
#' with GBM used to predict the response variable. 
#'
#' @format A Caret train object
"fit2"

#' Random forest fitted object from Caret on Sonar data with log-likelihood objective function
#'
#' Caret was run using 10 fold repeated cross validation on the Sonar data
#' using random forest to predict the response variable. Log-likelihood was set
#' to be the objective function to select the best model from cross validation.
#'
#' @format A Caret train object
"fit3"

#' Random forest fitted object from Caret on simulated imbalanced data
#'
#' Caret was run using 10 fold repeated cross validation on the Sonar data
#' with random forest to predict the response variable.
#'
#' @format A Caret train object
"im_fit"

#' Predictions from gbm on the Sonar test data
#'
#' The Sonar data was split into training (157 points) and 
#' testing (51 points), a gbm model was fitted using Caret 
#' on the training data. Then these are the predicted 
#' probabilities of the model on the test data.
#'
#' @format A data frame with 51 rows as points and 4 variables
"preds"

#' Predictions from gbm and random forest on the Sonar test data
#'
#' The Sonar data was split into training (157 points) and 
#' testing (51 points), a gbm model was fitted using Caret 
#' on the training data. Then these are the predicted 
#' probabilities of the model on the test data. A random
#' forest model was then fit and tested in the same manner. 
#' The probabilities and ground truth labels were combined 
#' horizontally for further analysis.
#'
#' @format A data frame with 102 rows as points and 4 variables
"predsc"