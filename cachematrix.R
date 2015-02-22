## This function will create a list that contains functions to set a matrix, get a matrix
## set the inverse of the matrix, and get the inverse of the matrix.  
## The <<- operator is used to cache the matrix and inverse to cache.

makeCacheMatrix <- function(x=matrix()) {
  ## Create a special "matrix" object that can cache its inverse
  
  xInverse <- NULL ## xInverse is the inverse of the matrix
  
  ## set function to set the value of the matrix
  set <- function(y) {
    x<<-y              ## Cache the value of the matrix
    xInverse <<-NULL   ## Cache the initialized value of the inverse
  }
  
  ## get function to get the value of the matrix
  get <- function() x
  
  ## set function to set the value of the inverse
  setInverse <- function(inverse) xInverse <<- inverse
  
  ## get function to get the value of the inverse
  getInverse <- function() xInverse
  
  ## list that contains these functions
  list (set=set,
        get=get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Function to calculate the inverse of a matrix
## It checks to see if the inverse is already calculated.  
## If it has, it retrieves the mean from cache and then skips the calculation.
## Otherwise, it calculates the inverse and sets the inverse value in cache.
cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInv()
  
  ## If it is null, it hasn't been calculated
  if (!is.null(xInverse)){
    message ("getting cached data")
    return (xInverse)
  }
  
  ## If is not null, it needs to be calculated
  data = x$get()
  xInverse <- solve(data)
  
  ## set the inverse of the matrix
  x$setInverse(xInverse)
  ## return the inverse
  xInverse
}