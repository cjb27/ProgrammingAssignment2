## Matrix inversion computations are costly from a computer resources 
## perspective.  This pair of functions runs more efficiently because it uses a cache 
##to find the inverse rather than computing it repeatedly.

##These are a pair of functions that cache the inverse of a matrix so 
##that it doesn't have compute every time

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  #stores a matrix
  setMatrix <- function(newV) {
    x <<- newV
    cache <<- NULL
  }
  
  #return a matrix that is stored
  getMatrix <- function() {
    x
  }
  
  #cache arguement
  cacheInverse <- function(solve) {
      cache <<- solve
  }
  
  # get cached value
  getInverse <- function() {
      cache
  }
  
  #returns a list of elements that are functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = 
         cacheInverse, getInverse = getInverse)
}

## This second function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cacheSolve fuction should 
##retrieve the inverse from the cache and returns a matrix that is the 
##inverse of 'x'

cacheSolve <- function(x, ...) {
    # get the cached value
    inverse <- y$getInverse()
    
    # if a cached value exists return cached value
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    # if cached value does not exist, get the matrix and caclulate the 
    #inverse and store it in the cache
    data <- y$getMatrix()
    inverse <- solve(data)
    y$cacheInverse(inverse)
    
    # return the inverse
    inverse
}
