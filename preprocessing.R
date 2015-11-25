# alleses <- read.csv("pollution.csv",header=TRUE,",")
alles  <- splitDate %.% read.csv("pollution.csv")

# put ozone into classes
alles$OZONE_CLASS <- cut(alles$O3, breaks=c(0,60.0,120.0,180.0,666),labels = c("LOW","MODERATE","HIGH","EXTREME"))
alles$PLARGE_CLASS <- cut(alles$PLARGE, breaks=c(0,35.0,50.0,666),labels = c("LOW","MODERATE","HIGH"))
alles$TLONG <- as.factor(alles$TLONG)
alles$TSHORT <- as.factor(alles$TSHORT)
clean <- alles[complete.cases(alles),]

target_variables <- c("OZONE_CLASS","O3","PLARGE","PLARGE_CLASS","PSMALL")
target_ozone <- c("PLARGE","PLARGE_CLASS","PSMALL")

dependant_variable_data <- excludeByColumn(alles,target_ozone)
nrow(dependant_variable_data)
clean <- excludeByColumn(alles[complete.cases(dependant_variable_data),], c("PLARGE","PSMALL","PLARGE_CLASS","PSMALL_CLASS"))
nrow(clean)
clean$DATE <- NULL
clean$O3 <- NULL

clean$RAIN[clean$RAIN > 100] <- 100
clean$WET_DAY <- clean$RAIN > 0



rowsWithNA<- alles[!complete.cases(alles),]

alles[1:10,]
summary(alles)

clean[1:10,]
summary(clean)

for (i in names(rows_with_na_except_class)){
    print("__________________________________________________")
    print(i)
    a <- sum(is.na(rows_with_na_except_class[i]))
    b <- nrow(rows_with_na_except_class) - a
    printf("%14.s has %5.1d na and %4.d not.na",i,a,b) # doesn't print string i :/
}

rows_with_na_except_class <- excludeByColumn(alles,c("OZONE_CLASS","O3"))
# rows_with_na_except_class <- excludeByColumn(alles,target_variables)
tmp <- complete.cases(alles[,names(alles) %in% c("OZONE_CLASS","O3")])
rows_with_na_except_class <- all[tmp,]



tmp <- alles[1:200,]
# plot(1:nrow(tmp),tmp$WIND,"l")
plot(1:(length(tmp$TSHORT)-1),abs(diff(forceNumeric(tmp$TLONG))),"l") # plots "derivative" of wind
