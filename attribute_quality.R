
clean <- ozone_clean

clean$RAIN2 <- log(1+clean$RAIN,2)
clean$WIND2 <- log(1+clean$WIND,2)
# clean$TEMP2 <- log(1+clean$HUM,2)
# clean$HUM2 <- log(1+clean$HUM,2)

clean$RAIN <- NULL
clean$WIND <- NULL

clean <- binarizeTraj(clean)


estimators <- c("InfGain","GainRatio","Gini","MDL","ReliefFequalK","ReliefFexpRank")
attribute_quality <- attrEvalMultipleEst("OZONE_CLASS",clean,estimators)
attribute_quality

attributeQualitySorted <- attrQualityVoting("OZONE_CLASS", clean, estimators = estimators)
attributeQualitySorted
