#Caching the Inverse of a Matrix 03/13/2016
#CM of United States of America

##Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly.

## This function is creates a special matrix object that 
## can cache its inverse

makeCacheMatrix <- function(xmat = matrix()) {
  ## x straight matrix - incoming argument
  imat <- NULL ## inverse matrix initialized to NULL
  
  ## set method intakes the new matrix
  set <- function(y) 
  {
    xmat <<- y  ## y is the new matrix 
    imat <<- NULL ## resets inverse matrix to null
  }
  
  ## returns the matrix when get called
  get <- function() xmat
  
  ## sets inversed matrix to solve
  setinverse <- function(inversedmatrix) 
  {
    imat <<- inversedmatrix
  }
  
  ## returns inversed matrix
  getinverse <- function() imat
  
  ## vector contains all the methods.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse 
## of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'xm'
  
  ## retrives the cached inversed matrix
  cachedimat <- x$getinverse()
  
  ## if the retrieval is successful returns inversed matrix.
  if(!is.null( cachedimat)) {
    message("Returning cached data")
    return(cachedimat)
  }
  
  ## retrieval failed proceed further
  
  ## get the matrix value.
  datamatrix <- x$get()
  
  ## inverse the matrix
  inversedmatrix <- solve(datamatrix, ...)
  
  ## store the invered matrix
  x$setinverse(inversedmatrix)
  
  ## display the inversed matrix
  inversedmatrix
  
}
