## Basic function as in the example of mean.
## However the CacheSolve function checks if the matrix is a square matrix.
## Only Square Matrix are Invertible
## functions do

## MakeCacheMatrix takes in a matrix as an argument and caches the matrix.
## the output of this function is list of 4 elements

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setmatrixinverse <- function(inverse) m <<- inverse
getmatrixinverse <- function() m
list(set = set, get = get, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse)
}


## Again using the sample program provided in the assignment
## replaced the mean with the SOLVE function.
## However before the Solve function there is condition to check if the matrix is a square matrix or not.
## only square matrix is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

## Condition to check for Square Matrix
  m1 <- dim(x$get())[1]
  m2 <- dim(x$get())[2]
  
  
  if (!is.na(ifelse(m1[1] == m2[2],1,0)))
  {return(m)}

    data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinverse(m)
  m
}
