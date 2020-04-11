## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an object with x variable to store matrix
# assumed to be a square matrix; inverse_x variable holds the value of 
# inverse of x. 'get' and 'set' functions are used to return 'x' or
# 'set' inverse_x to the inverse of x


## define a matrix object with setter and getter methods and variable
## x and inverse_x to store a square matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    ## assign 'y' to 'x' in parent env and 'NULL' to 'inverse_x'
    ## to reset value
    x <<- y
    inverse_x <<- NULL
  }
  ## return the matrix 'x'
  get <- function() x 

  ## Invert the matrix 'x' using 'solve'  
  setinverse <- function(solve) inverse_x <<- solve

  ## return 'inverse_x' value
  getinverse <- function() inverse_x 
  
  ## provide a list wiht named elements to allow use 
  ##of '$' to call funcitons
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the cached value of inverse of matrix if stored
## and tells user value is cached
## otherwise it calculates the inverse of matrix and sets this 
## in the cachedMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("Getting cached data")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$setinverse(inverse_x)
  inverse_x
}
