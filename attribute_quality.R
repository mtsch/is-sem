
clean <- clean_backup # backup

# tmp <- mdlp(clean[,!as.logical(lapply(clean,is.factor))])


clean <- binarizeTraj(clean)
"ReliefFbestK"

# estimators <- c("InfGain","GainRatio","Gini","MDL","ReliefFequalK","ReliefFexpRank", "ReliefFdistance", "DKM")
estimators <- c("ReliefFequalK","ReliefFexpRank", "ReliefFdistance")
# estimators <- c("ReliefFequalK","ReliefFexpRank","ReliefFbestK","ReliefFdistance")
attribute_quality <- attrEvalMultipleEst(target,clean,estimators)
attribute_quality

attributeQualitySorted <- attrQualityVoting(target, clean, estimators = estimators)
attributeQualitySorted

# clean2 <- clean[2000:2500,]
summary(clean)
tmp <- sampleClassesEqualDist(clean,target,ratio = 0.1, seed=42)
train <- tmp$train
test <- tmp$test

attrs <- names(attributeQualitySorted)
# attrs <- c("WAN","TOO","FREE","FOR","FIFE")
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

a <- as.numeric(results)
names(a) <- names(results)
b <- names(results)
c <- cbind(a)

b[with(b,order(-a))]

aaa <- includeByColumn(tmp$train, paste(c("TEMP","YDAY","TSHORT","HUM", "YEAR","TLONG","WIND2", target)))
summary(aaa)
aaaa <- includeByColumn(tmp$test, paste(c("TEMP","YDAY","TSHORT","HUM", "YEAR","TLONG","WIND2", target)))
summary(aaaa)
performClassification(PLARGE_CLASS ~ ., aaa, aaaa, "tree","ReliefFexpRank")[1]










attrs <- names(attributeQualitySorted)
for (i in names(attributeQualitySorted)){
    train <- includeByColumn(train, c(attrs, target))
    test  <- includeByColumn(test, c(attrs, target))
    accuracy <- performClassification(target, train, test, "tree","ReliefFexpRank")[1]
#     boost_model <- boosting(target %+%" ~ .", train, mfinal = 100)
#     pred <- predict.boosting(boost_model, test)
#     accuracy <- sum(pred$class == test[,target]) / nrow(test)
#     svm_model <- svm(target %+%" ~ .", train, scale=TRUE, kernel="polynomial")
#     pred <- predict(svm_model, test)
#     accuracy <- modelEval(model=NULL,test[,target], pred) # model is null because it's not a CORElearn model
#     accuracy[1]
    
    printf(attrs)
    printf("%f",accuracy)
    attrs <- attrs[-(length(attrs))]
}






boost_model <- boosting(target %+%" ~ .", train, mfinal = 100)
pred <- predict.boosting(boost_model, test)
accuracy <- sum(pred$class == test[,target]) / nrow(test)
accuracy

svm_model <- svm(OZONE_CLASS ~ ., train, scale=TRUE, kernel="polynomial")
pred <- predict(svm_model, test)
accuracy <- modelEval(model=NULL,test$OZONE_CLASS,pred) # model is null because it's not a CORElearn model
accuracy[1:8]

