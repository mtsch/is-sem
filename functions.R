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

performClassification <- function(class_name, train_data, test_data, model_name, predictionType="class", estimator_name=NULL, ...){
    if (is.null(estimator_name)){
        ml_model <- CoreModel(class_name, train_data, model=model_name, ...)
    } else{
        ml_model <- CoreModel(class_name, train_data, model=model_name, selectionEstimator=estimator_name, ...)
    }
    prediction <- predict(ml_model, test_data, type=predictionType)
    f_accuracies <- modelEval(ml_model, test_data[,class_name], prediction)
    list(accuracies=f_accuracies, model=ml_model, predictions=prediction)
}



excludeByColumn <- function(data, columns){
    data[, -which(names(data) %in% columns)]
}

includeByColumn <- function(data, columns){
    data[, which(names(data) %in% columns)]
}

sampleClassesEqualDist <- function(data, class_name, ratio=0.10, seed=0){
    if (!seed==0){
        set.seed(seed)
    }
    if (ratio > 1){
        ratio <- ratio / 100
    }
    n <- nrow(data)
    n_test <- round(n*ratio)
    shares <- table(data[,class_name])
    shares_sum <- sum(shares)
    test_shares <- round(n_test*(shares)/shares_sum+1) # +1 to make extreme have two samples
    train <- c()
    test <- c()
    for (i in names(shares)){
        sub_data <- data[data[class_name] == i,]
        sample <- sample(1:nrow(sub_data), test_shares[i])
        test <- rbind(test, sub_data[sample,])
        train <- rbind(train, sub_data[-sample,])
    }
    list(train=train, test=test)
    
}


attrQualityVoting <- function(target, data, estimators){
    attribute_quality <- attrEvalMultipleEst(target,data,estimators)
    names_without_target <- names(data)[names(data) != target]
    ranking <- numeric(length(names_without_target))
    names(ranking) <- names_without_target
    for (i in estimators){
        esti <- get(i,attribute_quality)
        for (j in names(data)){
            v <- match(j,names(esti))
            ranking[j] = ranking[j] + v
        }
    }
    ranking <- sort(ranking)
    ranking
}

increaseLowFreqData <- function(data, target, k=0.1){ # works like shit
    ratios_raw <- table(data[,target]) / nrow(data)
    ratios <- round(1/(ratios_raw^k) / min(1/(ratios_raw^k)))
    ratios <- ratios - min(ratios)
    ratios[ratios == Inf] <- 0
    data_names <- names(table(data[,target]))
    new_data <- data
    for(i in 1:length(data_names)){
        tmp <- data[data[,target] == data_names[i],]
        tmp2 <- tmp[rep(1:nrow(tmp),ratios[i]),]
        new_data <- rbind(new_data,tmp2)
        
    }
    new_data
}


predictionVoting <- function(predictions){
    votes <- array(0,dim(predictions[[1]]))
    for (i in predictions){
        votes <- votes + i
    }
    votes
    
}

