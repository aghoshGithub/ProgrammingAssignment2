## Two functions are defined to create a special object that
## stores a matrix and caches its inverse

## A matrix object is created which can cache its inverse by this function

makeCacheMatrix <- function(x = matrix()) {
Inv <- NULL

setIt <- function(a) {
  x <<- a
  Inv <<- NULL
}
getIt <- function() x
setInv <- function(Inverse) inv <<- inverse
getInv <- function() inv
list(setIt = setIt,
     getIt = getIt,
     setInv = setInv,
     getInv = getInv)
}



## THe ivnerse of "matrix" is computed by this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    return(Inv)
  }
  matr <- x$get()
  Inv <- solve(matr, ...)
  x$setInv(inv)
  Inv
}
