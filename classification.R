clean2 <- clean
clean2 <- excludeByColumn(clean2,c("YEAR","DAY","TLONG","RAIN"))
clean2 <- excludeByColumn(clean2,c("YDAY"))
test_sample <- sample(nrow(clean2),500)
test = clean2[test_sample,]
train = clean2[-test_sample,]
majority_classifier <- nrow(all[all$OZONE_CLASS == names(which.max(table(all$OZONE_CLASS))),]) / nrow(all)

tmp <- sampleClassesEqualDist(clean2,"OZONE_CLASS")
train <- tmp$train
test <- tmp$test


tic
models <- c("tree","rf","bayes","knn")
ests <- c("MDL","InfGain","GainRatio","ReliefFequalK","ReliefFexpRank")
accuracies <- data.frame()
for (i in models){ #needs 5-10 minutes to run
    if (i %in% c("tree","rf")){
        for (j in ests){
            accuracies[i,j] <- performClassification("OZONE_CLASS", train, test, i, j)[1] #selects only accuracy
        }
    }
    else {
        accuracies[i,1] <- performClassification("OZONE_CLASS", train, test, i)[1]
    }
    
}
toc
# load("accuracies_clean_data.Rda") #load accuracies from upper loops


boost_model <- boosting(OZONE_CLASS ~ .,train, mfinal = 100)
pred <- predict.boosting(boost_model, test)
accuracy <- sum(pred$class == test$OZONE_CLASS) / nrow(test)
accuracy
# 0.842 clean data
# 0.8364929 uniform dist clean data
# 0.8388626 removed YDAY
# 0.84 clean removed DAY YEAR RAIN TLONG
# 0.8246445 uniform dist removed DAY YEAR RAIN TLONG

bagg_model <- bagging(OZONE_CLASS ~ ., train, mfinal = 100)
pred <- predict.bagging(bagg_model,test)
accuracy <- sum(pred$class == test$OZONE_CLASS) / nrow(test)
accuracy
# 0.826 clean data
# 0.8151659 uniform dist clean data
# 0.8341232 removed YDAY 
# 0.838 clean removed DAY YEAR RAIN TLONG
# 0.8127962 uniform dist removed DAY YEAR RAIN TLONG

svm_model <- svm(OZONE_CLASS ~ ., train, scale=TRUE, kernel="polynomial")
pred <- predict(svm_model, test)
accuracy <- modelEval(model=NULL,test$OZONE_CLASS,pred) # model is null because it's not a CORElearn model
accuracy[1:8]
# 0.0.812 clean data 
# 0.0.7654028 uniform dist clean data # wrong, used clean as train
# 0.812 clean removed DAY YEAR RAIN TLONG
# 0.8104265 uniform dist removed DAY YEAR RAIN TLONG




