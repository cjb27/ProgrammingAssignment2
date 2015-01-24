##Matrix inversion is usually a costly computation.  This approach is more
##cost efficient because it looks to a cache first to find the inverse of 
##a matrix rather than computing it repeatedly.

## This first function creates a special "matrix" object that can cache its 
##inverse.

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
