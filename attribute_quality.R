
# clean <- clean_backup # backup

# tmp <- mdlp(clean[,!as.logical(lapply(clean,is.factor))])

# estimators <- c("InfGain","GainRatio","Gini","MDL","ReliefFequalK","ReliefFexpRank", "ReliefFdistance", "DKM")
# estimators <- c("ReliefFequalK","ReliefFexpRank", "ReliefFdistance")
# estimators <- c("ReliefFequalK","ReliefFexpRank","ReliefFbestK","ReliefFdistance")
# attribute_quality <- attrEvalMultipleEst(target,clean,estimators)
# attribute_quality
# 
# attributeQualitySorted <- attrQualityVoting(target, clean, estimators = estimators)
# attributeQualitySorted

# clean2 <- clean[2000:2500,]
# summary(clean)
# tmp <- sampleClassesEqualDist(clean,target,ratio = 0.1, seed=42)
# train <- tmp$train
# test <- tmp$test



# creates list with all combinations of attributes
attrs <- names(attributeQualitySorted) # insert appropriate data frame
variations_list <- list()
for (i in 1:length(attrs)){
    tmp2 <- combn(attrs,i)
    for (i in 1:ncol(tmp2)){
        variations_list[length(variations_list)+1] <- list(tmp2[,i])
    }
}


results <- vector()
for (i in variations_list){
    train <- includeByColumn(tmp$train, c(i, target))
    test  <- includeByColumn(tmp$test, c(i, target))
    accuracy <- performClassification(target, train, test, "tree","ReliefFexpRank")[1]
    #     boost_model <- boosting(target %+%" ~ .", train, mfinal = 100)
    #     pred <- predict.boosting(boost_model, test)
    #     accuracy <- sum(pred$class == test[,target]) / nrow(test)
    #     svm_model <- svm(target %+%" ~ .", train, scale=TRUE, kernel="polynomial")
    #     pred <- predict(svm_model, test)
    #     accuracy <- modelEval(model=NULL,test[,target], pred) # model is null because it's not a CORElearn model
    #     accuracy[1]
    results[length(results)+1] <- accuracy
    names(results)[length(results)] <- paste(i,collapse = " ")
    printf(i)
    printf("%f",accuracy)
    
}


# couldn't make it elegant
values <- as.numeric(results)
sort_order <- order(values,decreasing = TRUE)
names_values <- names(results)
values_sorted <- values[sort_order]
names_sorted <- names_values[sort_order]




