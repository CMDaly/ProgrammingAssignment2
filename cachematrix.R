## When running time consuming coputations or simpler computatios over large datasets
## it's a good idea to cache results so that they can be looked up later and
## re-used instead of computing again. This can reduce overall computation time.
## Matrix inversion is one such costly computation especially when performed insiide
## a loop. The functions below can perform and cache matirx inversion.

## The makeCacheMatrix function below creates a matrix which is passed to the
## cacheSolve function. 
## This returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##  This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function below coputes the inverse of the matrix from makeCacheMatrix
## but first checks to see if the inverse has already been computed. If it has
## it gets the inverse matrix from the cache, otherwise it computes the inverse of
## the matrix and sets the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## if the inverse has already been calculated retrieve it from the cache and 
  ## skip the computation.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse matrix has not been calculated, then calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  ## set the value of the inverse in the cache using the setinverse function.
  x$setinverse(inv)
  return(inv)
}
