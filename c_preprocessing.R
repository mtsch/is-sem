alles  <- splitDate %.% read.csv("pollution.csv")

# put ozone into classes
alles$OZONE_CLASS <- cut(alles$O3, breaks=c(0,60.0,120.0,180.0,666),labels = c("LOW","MODERATE","HIGH","EXTREME"))
alles$PLARGE_CLASS <- cut(alles$PLARGE, breaks=c(0,35.0,50.0,666),labels = c("LOW","MODERATE","HIGH"))
alles$TLONG <- as.factor(alles$TLONG)
alles$TSHORT <- as.factor(alles$TSHORT)
alles$RAIN[alles$RAIN > 100] <- 100
alles$WET_DAY <- as.factor(alles$RAIN > 0)

alles <- excludeByColumn(alles, c("DATE","O3","PLARGE","PSMALL"))

indexes_of_clean_ozone <- complete.cases(alles$OZONE_CLASS)
ozone_with_na <- excludeByColumn(alles[indexes_of_clean_ozone,], "PLARGE_CLASS") # semi clean data, removes plarge
ozone_clean <- replace.na.in.df(ozone_with_na)
summary(ozone_clean)


for (i in names(rows_with_na_except_class)){
    print("__________________________________________________")
    print(i)
    a <- sum(is.na(rows_with_na_except_class[i]))
    b <- nrow(rows_with_na_except_class) - a
    printf("%14.s has %5.1d na and %4.d not.na",i,a,b) # doesn't print string i :/
}


tmp <- alles[1:200,]
# plot(1:nrow(tmp),tmp$WIND,"l")
plot(1:(length(tmp$TSHORT)-1),abs(diff(forceNumeric(tmp$TLONG))),"l") # plots "derivative" of wind


