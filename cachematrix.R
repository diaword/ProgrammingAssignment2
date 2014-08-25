## rprog-006 week 2 assignment

## produces a matrix object capable of caching its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## inverse holder
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## returns inverse of a matrix, favoring the cached value if possible
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  message("no cached inverse found, dropping through to calculation")
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
