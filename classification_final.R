alles  <- splitDate %.% read.csv("pollution.csv")
alles$OZONE_CLASS <- cut(alles$O3, breaks=c(0,60.0,120.0,180.0,666),labels = c("LOW","MODERATE","HIGH","EXTREME"))
alles$PLARGE_CLASS <- cut(alles$PLARGE, breaks=c(0,35.0,50.0,666),labels = c("LOW","MODERATE","HIGH"))
alles$TLONG <- as.factor(alles$TLONG)
alles$TSHORT <- as.factor(alles$TSHORT)
alles$RAIN[alles$RAIN > 400] <- 400
alles <- excludeByColumn(alles, c("DATE","O3","PLARGE","PSMALL"))
target <- "PLARGE_CLASS"
anti_target <- "OZONE_CLASS"
target <- "OZONE_CLASS"
anti_target <- "PLARGE_CLASS"
summary(alles)
indexes_of_clean_data <- complete.cases(alles[,target])
data_with_na <- excludeByColumn(alles[indexes_of_clean_data,], anti_target) # removes unneeded class
clean <- replace.na.in.df(data_with_na)
summary(clean)
clean$RAIN2 <- log(1+clean$RAIN,2)
clean$WIND2 <- log(1+clean$WIND,2)
clean$RAIN <- NULL
clean$WIND <- NULL
clean_backup <- clean

majority_classifier <- max(table(clean[,target]))/nrow(clean)

sampled_data <- sampleClassesEqualDist(clean,target)
train <- sampled_data$train
test <- sampled_data$test



# ozone
model1 <- performClassification(target, train, test, "rf", "ReliefFexpRank")
n <- 1:nrow(clean)
samples <- numeric(10)

folds <- createFolds(1:nrow(clean), k=10, list=TRUE, returnTrain=FALSE)
accuracy <- 0
for (i in 1:10){
    train <- clean[-folds[[i]],]
    test <- clean[folds[[i]],]
    object <- performClassification(target,train,test,"rf","ReliefFequalK",predictionType="class")
    accuracy <- accuracy0 + object$accuracies[[1]]
}
accuracy <- accuracy / 10






# plarge
model1 <- performClassification(target, train, test, "rf", "ReliefFexpRank", rfNoTrees=500)
model1




test1 <- performClassification(target,train,test,"tree","ReliefFexpRank",predictionType="prob")
pred1 <- test1$predictions
test2 <- performClassification(target,train,test,"knn",predictionType="prob")
pred2 <- test2$predictions
test3 <- performClassification(target,train,test,"rf","ReliefFexpRank",predictionType="prob")
pred3 <- test3$predictions
test4 <- performClassification(target,train,test,"rf","ReliefFequalK",predictionType="prob")
pred4 <- test4$predictions

boost_model <- boosting(PLARGE_CLASS ~ ., train, mfinal = 100)
pred5_tmp <- predict.boosting(boost_model, test)
pred5 <- pred5_tmp$prob

bagg_model <- bagging(PLARGE_CLASS ~ ., train, mfinal = 100)
pred6_tmp <- predict.bagging(bagg_model,test)
pred6 <- pred6_tmp$prob

svm_model <- svm(PLARGE_CLASS ~ ., train, scale=TRUE, kernel="polynomial",probability=TRUE)
pred7_tmp <- e1071:::predict.svm(svm_model, test,probability=TRUE)
pred7 <- attr(pred7_tmp,"probabilities")



predx <- pred1 + pred2 + pred3 + pred4 + pred5 + pred6 + pred7



pred_list <- list(pred1,pred2,pred3,pred4,pred5,pred6,pred7,pred8)
pred_results <- predictionVoting(pred_list)
new_data <- cbind(pred_results,test$PLARGE_CLASS)
colnames(new_data)[length(colnames(new_data))] <- "PLARGE_CLASS"

new_data <- data.frame(new_data)
new_data$PLARGE_CLASS <- factor(new_data$PLARGE_CLASS,levels=c(1,2,3),labels=c("LOW","MODERATE","HIGH"))
sampled_data <- sampleClassesEqualDist(new_data,"PLARGE_CLASS",0.2)
new_train <- sampled_data$train
new_test <- sampled_data$test

test3 <- performClassification(target,new_train,new_test,"rf",estimator_name = "ReliefFexpRank",predictionType="prob")
pred3 <- test3$predictions
