## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix uses lexical scoping rules and stores matrices in memory

makeCacheMatrix <- function(x = matrix()) {
  # cache holds the cached value or NULL if nothing is cached
  #initially nothing is cached so set it to NULL  
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue){
    x <<- newValue
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() x
  
  #cache the given argument
  cacheInverse <- function(solve) inversecache <<- solve
  
  #get the cached value
  getInverse <- function() inversecache
  
  #each named element of the list is a function
  list(setMatrix=setMatrix,getMatrix=getMatrix,cacheInverse=cacheInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of a "special" matrix created with 
##makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the cached value
  inverse <- x$getInverse()
  
  #if a cached value exists return it
  if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  # otherwise get the matrix, calculate the inverse and store it in the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  #return the inverse matrix
  inverse  
}
