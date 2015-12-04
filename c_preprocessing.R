alles  <- splitDate %.% read.csv("pollution.csv")

# put ozone into classes
alles$OZONE_CLASS <- cut(alles$O3, breaks=c(0,60.0,120.0,180.0,666),labels = c("LOW","MODERATE","HIGH","EXTREME"))
alles$PLARGE_CLASS <- cut(alles$PLARGE, breaks=c(0,35.0,50.0,666),labels = c("LOW","MODERATE","HIGH"))
alles$TLONG <- as.factor(alles$TLONG)
alles$TSHORT <- as.factor(alles$TSHORT)
alles$RAIN[alles$RAIN > 100] <- 100
#alles$WET_DAY <- as.factor(alles$RAIN > 0)

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
# clean$TEMP2 <- log(1+clean$HUM,2)
# clean$HUM2 <- log(1+clean$HUM,2)
clean$RAIN <- NULL
clean$WIND <- NULL

clean_backup <- clean










for (i in names(rows_with_na_except_class)){
    print("__________________________________________________")
    print(i)
    a <- sum(is.na(rows_with_na_except_class[i]))
    b <- nrow(rows_with_na_except_class) - a
    printf("%14.s has %5.1d na and %4.d not.na",i,a,b) # doesn't print string i :/
}


# solar angle
clean2$SOLAR_ANGLE <- 23.45*(pi/180)*sin(2*pi*((284+clean$YDAY)/36.25)) 
clean2$SOLAR_ANGLE2 <- 34+sin(pi*8*clean$YDAY/80) # dummy
