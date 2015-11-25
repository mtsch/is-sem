test_sample <- sample(nrow(clean),500)
test = clean[test_sample,]
train = clean[-test_sample,]
majority_classifier <- nrow(all[all$OZONE_CLASS == names(which.max(table(all$OZONE_CLASS))),]) / nrow(all)



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

load("accuracies_clean_data.Rda") #load accuracies from upper loops


boost_model <- boosting(OZONE_CLASS ~ .,train)
pred <- predict.boosting(boost_model, test)
accuracy <- sum(pred$class == test$OZONE_CLASS) / nrow(test)
# 0.842 clean data


bagg_model <- bagging(OZONE_CLASS ~ ., train)
pred <- predict.bagging(bagg_model,test)
accuracy <- sum(pred$class == test$OZONE_CLASS) / nrow(test)
# 0.826 clean data

