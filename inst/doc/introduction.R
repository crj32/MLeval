## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.width=5,fig.height=5--------------------------------------------
library(MLeval)

## ordinary input
head(preds)
  
## run MLeval
test1 <- evalm(preds,plots='r',rlinethick=0.8,fsize=8,bins=8)

## ----fig.width=5,fig.height=5--------------------------------------------
test1$stdres

## ----fig.width=5,fig.height=5--------------------------------------------
test1$optres

## ----fig.width=5,fig.height=3.5------------------------------------------
test1$cc

## ----fig.width=5,fig.height=3--------------------------------------------
head(test1$probs[[1]])

## ----fig.width=5,fig.height=5--------------------------------------------
library(MLeval)

## 
levels(as.factor(predsc$Group))
head(predsc)
  
## run MLeval
test1 <- evalm(predsc,plots='r',rlinethick=0.8,fsize=8,bins=8)

## ----fig.width=5,fig.height=5--------------------------------------------
library(MLeval)

## run cross validation on Sonar data
# fitControl <- trainControl(
#   method = "repeatedcv",
#   summaryFunction=twoClassSummary,
#   classProbs=T,
#   savePredictions = T)
# fit1 <- train(Class ~ ., data = Sonar, 
#                method = "ranger", 
#                trControl = fitControl,metric = "ROC",
#                verbose = FALSE)

## evaluate
test1 <- evalm(fit1,plots='r',rlinethick=0.8,fsize=8)

## ----fig.width=5,fig.height=5--------------------------------------------
library(MLeval)

# Caret train function output object -- repeated cross validation
# run caret
# fitControl <- trainControl(
#   method = "repeatedcv",
#   summaryFunction=twoClassSummary,
#   classProbs=T,
#   savePredictions = T)
# fit2 <- train(Class ~ ., data = Sonar, 
#              method = "gbm", 
#              trControl = fitControl,metric = "ROC",
#              verbose = FALSE)

# plot rocs
test4 <- evalm(list(fit1,fit2),gnames=c('ranger','gbm'),rlinethick=0.8,fsize=8,
              plots='r')

## ----fig.width=5,fig.height=5--------------------------------------------
test4$optres

## ----fig.width=5,fig.height=5--------------------------------------------
library(MLeval)

# im <- twoClassSim(2000, intercept = -25, linearVars = 20)
# table(im$Class)
# 
# fitControl <- trainControl(
#   method = "cv",
#   summaryFunction=prSummary,
#   classProbs=T,
#   savePredictions = T,
#   verboseIter = F)
# im_fit <- train(Class ~ ., data = im,
#                   method = "ranger",
#                   metric = "AUC",
#                   trControl = fitControl)

x <- evalm(im_fit,rlinethick=0.8,fsize=8,plots=c())

## ----fig.width=5,fig.height=5--------------------------------------------
x$optres

## ----fig.width=5,fig.height=5--------------------------------------------
x$roc

## ----fig.width=5,fig.height=5--------------------------------------------
x$proc

## ----fig.width=5,fig.height=5--------------------------------------------
x$prg

## ----fig.width=5,fig.height=5--------------------------------------------
library(MLeval)

# # set up custom function for the log likelihood
# LLSummary <- function(data, lev = NULL, model = NULL){
#   LLi <- LL(data,positive='R')
#   names(LLi) <- "LL"
#   out <- LLi
#   out
# }
# 
# fitControl <- trainControl(
#   method = "cv",
#   summaryFunction = LLSummary,
#   classProbs=T,
#   savePredictions = T,
#   verboseIter = T)
# fit3 <- train(Class ~ ., data = Sonar,
#              method = "ranger",
#              trControl = fitControl,metric = "LL",
#              verbose = FALSE)

y <- evalm(fit3,rlinethick=0.8,fsize=8,plots=c('prg'))

## ----fig.width=5,fig.height=3.5------------------------------------------
y$cc

