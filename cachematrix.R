makeVector <- function(x = numeric()) {
  inv <- NULL  ##initializing inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## function to get matrix
  setmean <- function(mean) inv <<- mean
  getmean <- function() inv   ## function to get the inverse of the matrix
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) { ## get cache data
  m <- x$getmean()
  if(!is.null(inv)) {  ## checking if inverse is null
    message("getting cached data")
    return(inv) ## returns inverse value  
  }
  data <- x$get() # calculates inverse value
  m <- mean(data, ...)
  x$setmean(inv)
  m
}
