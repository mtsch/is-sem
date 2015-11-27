clean2 <- clean # backup
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



clean2 <- clean # backup
clean2 <- excludeByColumn(clean2, c("WET_DAY","TLONG","TSHORT","MONTH","RAIN","DAY"))
tmp <- sampleClassesEqualDist(clean2,"OZONE_CLASS")
train <- tmp$train
test <- tmp$test

# ozone_index <- which(names(train) == "OZONE_CLASS")
# train_ozone <- train_matrix[,"OZONE_CLASS"]
# train_ozone <- as.matrix(unclass(as.factor(train_ozone)))
# train_matrix <- as.matrix(train)
# train_matrix[,"WET_DAY"] <- unclass(as.factor(train_matrix[,"WET_DAY"]))
# train_matrix[,"TLONG"] <- unclass(as.factor(train_matrix[,"TLONG"]))
# train_matrix[,"TSHORT"] <- unclass(as.factor(train_matrix[,"TSHORT"]))
# train_matrix[,"OZONE_CLASS"] <- unclass(as.factor(train_matrix[,"OZONE_CLASS"]))


# another try
train_ozone <- data.matrix(unclass(as.factor(train[,"OZONE_CLASS"])))
train_matrix <- data.matrix(train)
test_ozone <- data.matrix(unclass(as.factor(test[,"OZONE_CLASS"])))
test_matrix <- data.matrix(test)

train_matrix <- train_matrix[,-6] # need function to remove column from name
test_matrix <- test_matrix[,-6]

mp_weights <- monmlp.fit(x = train_matrix,y =  train_ozone, hidden1 = 6, hidden2 = 6)
pred <- monmlp.predict(x = as.matrix(test_matrix), weights = mp_weights)
pred2 <- factor(round(pred),levels = c(1,2,3,4), labels = c("LOW","MODERATE","HIGH","EXTREME"))
accuracy <- modelEval(model=NULL,test$OZONE_CLASS,pred2)
accuracy[1:8]
# using all attributes
# hidden1=2 0.8199052
# hidden1=10 0.8341232
# hidden1=20 0.7914692
# hidden1=2 hidden2=2 0.8270142
# hidden1=5 hidden2=5 0.7938389
# hidden1=10 hidden2=10 0.8104265
# hidden1=20 hidden2=20 0.7511848, contained 1 NA because 1 value was below 0.5

# removed c("WET_DAY","TLONG","TSHORT","MONTH","RAIN","DAY")
# h1=2 0.7796209
# h1=10 0.8080569
# hidden1=20 0.8127962
# hidden1=2 hidden2=2 0.7772512
# hidden1=5 hidden2=5 0.8127962
# hidden1=10 hidden2=10 0.7914692
# hidden1=20 hidden2=20 0.7511848




