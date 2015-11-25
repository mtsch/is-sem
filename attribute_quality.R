
estimators <- c("InfGain","GainRatio","Gini","MDL","ReliefFequalK","ReliefFexpRank")
attribute_quality <- attrEvalMultipleEst("OZONE_CLASS",clean,estimators)
attribute_quality
