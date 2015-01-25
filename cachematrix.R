# purpose of these functions is to calculate and store
# in cache in the inverse of a matrix

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. 

# makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
  
  # holds the cached value or NULL if nothing is cached null to start
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # clear cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # else get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
