# data prep
library(pls)
library(e1071)
library(kknn)
library(randomForest)
library(CORElearn)
library(nnet)

prepData <- function( all.df, output.col, exclude.col
                    , test.percent=0.1, impute=imputeMean, log.out=F, log.rw=F)
{

  # rm na in output
  df <- all.df[!is.na(all.df[, output.col]), ]
  # rm exclude.col
  df <- df[, !names(df) %in% c(exclude.col, "DAY")]
  # binarize TRAJ
  df <- binarizeTraj(df)
  # winsorize RAIN
  max.rain <- mean(df$RAIN) + sd(df$RAIN)
  df$RAIN[df$RAIN > max.rain] <- max.rain

  # impute missing values in inputs
  cnames    <- colnames(df)
  resp.i    <- which(cnames == output.col)
  imputed.x <- impute(df[, -resp.i])
  df        <- named(cbind(imputed.x, df[, resp.i]), col=cnames)

  if (log.out)
    df[, output.col] <- log(df[, output.col] + 1)

  if (log.rw) {
    df$WIND <- log(1 + df$WIND)
    df$RAIN <- log(1 + df$RAIN)
  }

  # split data into test and learn data
  n    <- nrow(df)
  test <- sample(1:n, round(n * test.percent))

  list( learn = df[-test, ]
      , test  = df[ test, ] 
      , all   = df )

}

imputeMean <- function(df)
{
  as.data.frame(lapply(df, function(v) {v[is.na(v)] <- mean(v, na.rm=T); v}))
}

# ayy nno tole dela bolš brez tega
imputeFun <- function(df, fun=randomForest)
{
  df.learn <- imputeMean(df)

  imputeCol <- function(index) {
    out <- df[, index]
    if (any %.% is.na(out)) {
      message(index %+% "/" %+% ncol(df) %+%
              ": Imputing " %+% colnames(df)[index] %+% "...")

      x <- df.learn[, -index]
      y <- df.learn[,  index]

      model <- fun(x, y)
      na.f  <- is.na(out)

      out[na.f] <- predict(model, df.learn[na.f, -index])
      out
    } else {
      out
    }
  }

  imputed <- as.data.frame %.% lapply(1:ncol(df), imputeCol)
  named(imputed, col=names(df))
}

# poslabša RF, PLS, KNN PSMALL
# izboljša SVM (minimalno), KNN O3
rrelieffFilter <- function(formula, dfs, thresh=0, est="RReliefFequalK")
{
  evald <- attrEval(formula, data=dfs$all, estimator=est)
  filt  <- c(evald >= thresh, TRUE)

  out.name <- as.character(formula[2])
  #lapply(dfs, function(df)
      #named( cbind(df[, filt], df[, out.name])
           #, col=c(colnames(df[, filt]), out.name)))
  lapply(dfs, function(df) df[, filt])
}

getCor <- function(model, dfs, response, ...)
{
  cor(predict(model, newdata=dfs$test, ...), dfs$test[, response])
}

performPLS <- function(O3dfs, PSdfs, ...)
{
  O3p <- plsr(O3 ~ .,     data=O3dfs$learn, validation="CV", ...)
  PSp <- plsr(PSMALL ~ ., data=PSdfs$learn, validation="CV", ...)

  # zadeva je na 03 kr ok, na PSMALL pa smrdi
  plot(O3p, asp=1, line=T)
  plot(PSp, asp=1, line=T)
  plot(RMSEP(O3p), legendpos="topright")
  plot(RMSEP(PSp), legendpos="topright")

  summary(O3p)
  summary(PSp)

  list( O3 = O3p, PS = PSp
      , O3cor = getCor(O3p, O3dfs, "O3", ncomp=3)
      , PScor = getCor(PSp, PSdfs, "PSMALL", ncomp=7))
}

performSVM <- function(O3dfs, PSdfs, ...)
{
  O3s <- best.svm(O3 ~ .,     data=O3dfs$learn, nrepeat=10, ...)
  PSs <- best.svm(PSMALL ~ ., data=PSdfs$learn, nrepeat=10, ...)

  summary(O3s)
  summary(PSs)

  list ( O3 = O3s, PS = PSs
       , O3cor = getCor(O3s, O3dfs, "O3")
       , PScor = getCor(PSs, PSdfs, "PSMALL"))
}

performKNN <- function(O3dfs, PSdfs, dist=1, ker="gauss", ...)
  # dobri kerneli - gauss, triangular, epanechnikov...
{
  O3k <- kknn( O3 ~ .
             , train=O3dfs$learn, test=O3dfs$test
             , distance=dist, kernel=ker, ...)
  PSk <- kknn( PSMALL ~ .
             , train=PSdfs$learn, test=PSdfs$test
             , distance=dist, kernel=ker, ...)

  summary(O3k)
  summary(PSk)

  list ( O3 = O3k, PS = PSk
       , O3cor = getCor(O3k, O3dfs, "O3")
       , PScor = getCor(PSk, PSdfs, "PSMALL"))
}

performRF <- function(O3dfs, PSdfs, ..., best=F) # best ni zares blazno boljši
{
  if (best) {
    O3r <- best.randomForest(O3 ~ ., data=O3dfs$learn, ...)
    PSr <- best.randomForest(PSMALL ~ ., data=PSdfs$learn, ...)
  } else {
    O3r <- randomForest(O3 ~ ., data=O3dfs$learn, ...)
    PSr <- randomForest(PSMALL ~ ., data=PSdfs$learn, ...)
  }

  summary(O3r)
  summary(PSr)
  
  list ( O3 = O3r, PS = PSr
       , O3cor = getCor(O3r, O3dfs, "O3")
       , PScor = getCor(PSr, PSdfs, "PSMALL"))
}

performANN <- function(O3dfs, PSdfs, best=F, size=15, ...)
{
  if (best) {
    O3a <- best.nnet(O3 ~ ., data=O3dfs$learn, maxit=1000, size=size, ...)
    print("ping!")
    PSa <- best.nnet(PSMALL ~ ., data=PSdfs$learn, maxit=1000, size=size, ...)
    print("pong!")
  } else {
    O3a <- nnet(O3 ~ ., data=O3dfs$learn, size=size, decay=1, maxit=1000, ...)
    PSa <- nnet(PSMALL ~ ., data=PSdfs$learn, size=size, decay=1, maxit=1000, ...)
  }

  summary(O3a)
  summary(PSa)
  
  list ( O3 = O3a, PS = PSa
       , O3cor = getCor(O3a, O3dfs, "O3")
       , PScor = getCor(PSa, PSdfs, "PSMALL"))
}

O3.data.me <- prepData( all.df
                      , "O3", c("PLARGE", "PSMALL", "DATE"))
PS.data.me <- prepData( all.df
                      , "PSMALL", c("PLARGE", "O3", "DATE"))
PS.data.me.log <- prepData( all.df
                          , "PSMALL", c("PLARGE", "O3", "DATE"), log.out=T)
# log mara svm
O3.data.log <- prepData( all.df
                        , "O3", c("PLARGE", "PSMALL", "DATE"), log.rw=T)
PS.data.log <- prepData( all.df
                        , "PSMALL", c("PLARGE", "O3", "DATE"), log.rw=T)

if (F){
O3.data.rf <- prepData( all.df
                      , "O3", c("PLARGE", "PSMALL", "DATE"), impute=imputeFun)
PS.data.rf <- prepData( all.df
                      , "PSMALL", c("PLARGE", "O3", "DATE"), impute=imputeFun)
O3.data.scaled <- lapply(O3.data.me, as.data.frame %.% scale)
PS.data.scaled <- lapply(PS.data.me, as.data.frame %.% scale)

resultO3 <- c()
resultPS <- c()

for (i in 1:30*2) {
  print(i)
  ann <- performANN(O3.data.scaled, PS.data.scaled, size=i)
  resultO3[i/2] <- ann$O3cor
  resultPS[i/2] <- ann$PScor
}
}
