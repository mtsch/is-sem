alles  <- splitDate %.% read.csv("pollution.csv")
alles$OZONE_CLASS <- cut(alles$O3, breaks=c(0,60.0,120.0,180.0,666),labels = c("LOW","MODERATE","HIGH","EXTREME"))
alles$PLARGE_CLASS <- cut(alles$PLARGE, breaks=c(0,35.0,50.0,666),labels = c("LOW","MODERATE","HIGH"))
alles$TLONG <- as.factor(alles$TLONG)
alles$TSHORT <- as.factor(alles$TSHORT)
alles$RAIN[alles$RAIN > 400] <- 400
alles <- excludeByColumn(alles, c("DATE","O3","PLARGE","PSMALL","MONTH","DAY"))
alles$RAIN2 <- log(1+alles$RAIN,2)
alles$WIND2 <- log(1+alles$WIND,2)
alles$RAIN <- NULL
alles$WIND <- NULL
target <- "PLARGE_CLASS"
anti_target <- "OZONE_CLASS"
target <- "OZONE_CLASS"
anti_target <- "PLARGE_CLASS"
indexes_of_clean_data <- complete.cases(alles[,target])

indexes_of_clean_data <- complete.cases(alles)
data_with_na <- excludeByColumn(alles[indexes_of_clean_data,], anti_target) # removes unneeded class
clean <- replace.na.in.df(data_with_na)
clean_backup <- clean

majority_classifier <- max(table(clean[,target]))/nrow(clean)
# ozone 0.6902685
# plarge 0.9751678



# ozone
folds <- createFolds(1:nrow(clean), k=10, list=TRUE, returnTrain=FALSE)
accuracy <- 0
for (i in 1:10){
    train <- clean[-folds[[i]],]
    test <- clean[folds[[i]],]
    object <- performClassification(target,train,test,"rf",estimator_name = "ReliefFexpRank", rfNoTrees=100)
    accuracy <- accuracy + object$accuracies[[1]]
    print(accuracy/i)
}
accuracy <- accuracy / 10
accuracy






sampled_data <- sampleClassesEqualDist(clean,target)
train <- sampled_data$train
test <- sampled_data$test


.


rf_model <- CoreModel(OZONE_CLASS ~ ., train, model="rf", selectionEstimator="ReliefFexpRank")
pred_rf <- predict(rf_model, test, type="class")
accuracy_rf <- sum(test$OZONE_CLASS == pred_rf)/nrow(test)
accuracy_rf # = 0.8443709 

train <- excludeByColumn(train,c("YEAR","TLONG","RAIN2"))
test <- excludeByColumn(test,c("YEAR","TLONG","RAIN2"))
boost_model <- adabag::boosting(OZONE_CLASS ~ ., train, mfinal = 100)
pred_boost <- predict.boosting(boost_model, test)
accuracy_boost <- sum(test$OZONE_CLASS == pred_boost$class)/nrow(test)
accuracy_boost # = 0.8443709

bagg_model <- adabag::bagging(OZONE_CLASS ~ ., train, mfinal = 100)
pred_bagg <- predict.bagging(bagg_model,test)
accuracy_bagg <- sum(test$OZONE_CLASS == pred_bagg$class)/nrow(test)
accuracy_bagg # = 0.8344371















# plarge rf 10 fold 0.9748398
rf_model <- CoreModel(PLARGE_CLASS ~ ., train, model="rf", selectionEstimator="MDL", rfNoTrees=100)
pred_rf <- predict(rf_model, test, type="class")
accuracy_rf <- sum(test$PLARGE_CLASS == pred_rf)/nrow(test)
accuracy_rf #  


boost_model <- adabag::boosting(PLARGE_CLASS ~ ., train, mfinal = 100)
pred_boost <- predict.boosting(boost_model, test)
accuracy_boost <- sum(test$PLARGE_CLASS == pred_boost$class)/nrow(test)
accuracy_boost # 

bagg_model <- adabag::bagging(PLARGE_CLASS ~ ., train, mfinal = 100)
pred_bagg <- predict.bagging(bagg_model,test)
accuracy_bagg <- sum(test$PLARGE_CLASS == pred_bagg$class)/nrow(test)
accuracy_bagg # 








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




