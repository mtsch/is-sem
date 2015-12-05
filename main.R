library(CORElearn) # tree, knn, bayes, rf, regtree https://cran.r-project.org/web/packages/CORElearn/CORElearn.pdf
library(plyr)
library(rpart)
library(adabag) # boosting and bagging    https://cran.r-project.org/web/packages/e1071/e1071.pdf
library(e1071) # SVM    https://cran.r-project.org/web/packages/e1071/e1071.pdf
library(tictoc)
library(monmlp) # multilayer perceptron    https://cran.r-project.org/web/packages/monmlp/monmlp.pdf
#library(weka) # may be useful, not sure
library(caret) 
# library(xlxs) # may be needed for rJava - or maybe not
library(rJava) # for FSelector
library(FSelector) # feature selection 
library(discretization) # https://cran.r-project.org/web/packages/discretization/discretization.pdf
#library(infotheo) # discretization https://cran.r-project.org/web/packages/infotheo/infotheo.pdf
library(kernlab)
library(nnet)
library(caret) # pca and other https://cran.r-project.org/web/packages/caret/caret.pdf
library(rmarkdown)


source("helpers.R")
source("exploration.R")
source("functions.R")
# source("preprocessing.R") # commented for now

