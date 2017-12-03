## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL    # Reserve empty value for matrix inverse
  set <- function(y) {    # Creates the set function that sets new value of matrix 
    x <<- y
    inverseM <<- NULL    # If the new matrix exists, re-sets inverseM to NULL
  }
  get <- function() x    # Creates the get function that assings new value of matrix
  setInverse <- function(inverse) inverseM <<- inverse    # Sets the value of the inverse
  getInverse <- function() inverseM    # Gets the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# Using makeCacheMatrix function gets the value of the invertible matrix
  inverseM <- x$getInverse()
    if(!is.null(inverseM)) {    # If inverseM already exists
      message("Getting cached data")   # Return message
      return(inverseM)    # and return inverseM
    }
# In the case if inverseM is NULL
    data <- x$get()    # Gets the matrix data
    inverseM <- solve(data, ...)    # Computing the inverse of matrix by the solve function
    x$setInverse(inverseM)    # Sets the invertible matrix
    inverseM    # Returns the invertible matrix
}
  

