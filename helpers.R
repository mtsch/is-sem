# paste operator
`%+%` <- paste0

# function composition
`%.%` <- function(f, g) { if (is.function(g))
                            function(...) f(g(...))
                          else
                            f(g) }

# close all devices
closeall <- function() while(T) dev.off()

# force conversion to numeric
forceNumeric <- suppressWarnings %.% as.numeric

# curry a function. borrowed from library(functional)
curry <- function(f, ...)
{
  args <- list(...)
  function(...) do.call(f, c(args, list(...)))
}

# add names to object
named <- function(obj, ..., row=dimnames(obj)[[1]], col=dimnames(obj)[[2]])
{
  if (is.null %.% dim(obj))
    names(obj) <- c(...)
  else
    dimnames(obj) <- list(row, col)
  obj
}

# repeat vector by rows
  # a vec, num, log -> a mat
repMat <- function(vec, n, byrow=F)
{
  if (byrow)
    matrix(rep(vec, n), nrow=n, byrow=T)
  else
    matrix(rep(vec, n), ncol=n, byrow=F)
}

# directory paste operator
`%/%` <- function(s1, s2)
{
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  l1 <- nchar(s1)
  l2 <- nchar(s2)
  s1 <- ifelse( substr(s1, l1, l1) == "/"
              , substr(s1, 1, l1-1)
              , s1 )
  s2 <- ifelse( substr(s2, 1, 1) == "/"
              , substr(s2, 2, l2)
              , s2 )
  paste(s1, s2, sep="/")
}




printf <- function(...) invisible(print(sprintf(...)))

