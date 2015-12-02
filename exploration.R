library(plyr)
source("helpers.R")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DATE           : Measurement date
# TRAJ           : Nominal description of 7 day air mass movement
# SHORT_TRAJ     : Nominal description of 1-2 day air mass movement
# AMP_TMP2M_mean : Mean temp
# AMP_RH_mean    : Humidity
# AMP_WS_mean    : Wind speed
# AMP_PREC_sum   : Total precipitation
# O3         : Max ozone level          reg.
# PLARGE           : Large particle conc.
# PSMALL          : Small particle conc.     reg.
# # ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# podatki kjer so vsi trije outputi NA so bv
filterNAOuts <- function(df)
{
  num <- ncol(df)
  dat <- as.matrix(df[, (num-2):num])
  fil <- aaply(dat, 1, all %.% is.na)

  df[!fil, ]
}

# zamenja DATE z YDAY in YEAR
splitDate <- function(df)
{
  dates <- as.POSIXlt(df$DATE, format="%Y-%m-%d")
  ydays <- dates$yday
  years <- dates$year
  months <- dates$mon
  days   <- dates$mday

  cbind(YEAR=years, YDAY=ydays, MONTH=months, DAY=days, df)
}

# naštima podatke za klasifikacijo
prepareClassData <- function(df, target.col, breaks, labels, remove.cols)
{
  df[, target.col] <- cut( df[, target.col]
                         , breaks=breaks
                         , labels=labels )

  df <- df[, !names(df) %in% remove.cols]
  df[!is.na(df[, target.col]), ]
}

prepareOzoneClassData <- function(df)
{
  prepareClassData( df
                  , "O3"
                  , c(0, 60, 120, 180, max(df$O3, na.rm=T))
                  , c("LOW", "MODERATE", "HIGH", "EXTREME")
                  , c("PLARGE", "PSMALL") )
}

preparePLARGEClassData <- function(df)
{
  prepareClassData( df
                  , "PLARGE"
                  , c(0, 35, 50, max(df$PLARGE, na.rm=T))
                  , c("LOW", "MODERATE", "HIGH")
                  , c("O3", "PSMALL") )
}

# plot podatkov za klasifikacijo. prčakuje se da je v zadnjem stolpcu
plotClassData <- function(df, pch=".", ...)
{
  outs   <- df[, ncol(df)]
  colors <- rainbow(nlevels(outs))

  plot(df, pch=pch, col=colors[outs], ...)
}

# binarizacija vektorja
binarizeVector <- function(v, name)
{
  v <- as.factor(v)
  named( t %.% aaply(1:nlevels(v), 1, function(i, x) as.numeric(i == x), v)
       , col = name %+% ".is." %+% levels(v) )
}

# binarizira traj in short traj
binarizeTraj <- function(df)
{
  t.long  <- binarizeVector(df$TLONG, "TLONG")
  t.short <- binarizeVector(df$TSHORT, "TSHORT")

  df$TLONG  <- NULL
  df$TSHORT <- NULL

  cbind(t.long, t.short, df)
}

all.df  <- splitDate %.% read.csv("pollution.csv") # vsi podatki
full.df <- all.df[complete.cases(all.df), ]        # vsi nemankajoči podatki

O3.df     <- prepareOzoneClassData(all.df)
PLARGE.df <- preparePLARGEClassData(all.df)


replace.na <- function(v)
{
#     if(!any(is.na(v))){ # there was an error with OZONE_CLASS 
#         
#     }
    if (is.numeric(v))
        v[is.na(v)] <- mean(v, na.rm=T)
    else
        v[is.na(v)] <- names(which.max(table(v)))
    v
}

replace.na.in.df <- function(data) {
    for (i in names(data)){
        data[,i] <- replace.na(data[,i])
    }
    data
}
