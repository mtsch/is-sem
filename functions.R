attrEvalAndPlot <- function(class, data, estimator)
{
    values <- attrEvalSort(class, data, estimator)
    attrEvalPlot(values, estimator)
}

attrEvalSort <- function(class, data, estimator)
{
    values <- attrEval(class,data,estimator=estimator)
    values <- sort(values,decreasing = TRUE)
#     "InfGain" # bigger=better, 0<=infgain<=entropy
#     "GainRatio" # bigger=better, 0<=gainratio<=norm_entropy
#     "Gini" # bigger=better, ???
#     "MDL" # bigger=better, ???
#     "ReliefFequalK" # probably bigger=better
}

attrEvalPlot <- function(values, estimator)
{
#     if(length(values) < length(names))
#     {
#         names <- names[-length(names)]
#     }
    values <- sort(values,decreasing=TRUE)
    plot(values,xlab="Attribute",ylab="Value",xaxt="n",main=estimator)   # Matija, prosim popravi, da bojo stevilke lepo not
    axis(side=1,at=1:length(values),labels=names(values))
    text(1:length(values),values,labels=round(values,3),pos=1)
    values
        
}
sortDFByColumn <- function(data, column, decreasing=FALSE)
{
    if (decreasing)
        column = -column
    data[with(data, order(column)),]
}

attrEvalMultipleEst <- function(class, data, estimators){
    values <- list()
    for (i in estimators){ 
        values[i] <- list(attrEvalSort(class, data, estimator=i))
    
    }
    values
}

performClassification <- function(class_name, train_data, test_data, model_name, estimator_name=NULL){
    if (is.null(estimator_name)){
        ml_model <- CoreModel(class_name, train_data, model=model_name)
    }
    else{
        ml_model <- CoreModel(class_name, train_data, model=model_name, selectionEstimator=estimator_name)
    }
    prediction <- predict(ml_model, test_data, type="class")
    f_accuracies <- modelEval(ml_model, test[,class_name], prediction)
    f_accuracies
    # list(accuracies=f_accuracies, model=ml_model)
}
# tree_m <- CoreModel("OZONE_CLASS",train,model="tree")
# tree_pred <- predict(tree_m,test, type="class")
# accuracies <- modelEval(tree_m,test$OZONE_CLASS,tree_pred)
# accuracies[1:7]


excludeByColumn <- function(data, columns){
    data[, -which(names(data) %in% columns)]
}


