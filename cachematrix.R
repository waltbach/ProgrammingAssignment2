# Overview
# These functions provide a facility that can be used to cache a matrix and it's
# inverse. Once the inverse of the matrix has been calculated, it can be retrieved
# without recalculating. The makeCacheMatrix function creates the object 
# containing the matrix and a cached matrix (initally NULL) This object provides 
# methods to retrieve the matrix and the cached matrix (get and getCached) and also a 
# method to change the base matrix and save the cached matrix (set and setCached). 
# The cacheSolve function, when passed the an object created by makeCacheMatrix
# returns the inverse matrix that is stored in that object. If the object does not
# have an inverse matrix, it is first calcualted and stored in the object.


# makeCacheMatrix
# Creates an object that will hold a matrix and it's cached inverse with 
# methods (functions) to update and retrieve the base matrix and it's cached inverse. 
# The cacheSolve functions should be used to create the cached inverse.
# 
# Args:
#   x: a matrix
# 
# Returns: 
#   An object containing the input matrix (base) and a NULL cached matrix
#   Four functions to access the base matrix and the cached matrix.
#     get           - returns the base matrix
#     set(y)        - save matrix y as the base and set the cached matrix to NULL 
#     getCached     - returns the cached matrix
#     setCached(y)  - save matrix y into the cache 

makeCacheMatrix <- function(x = matrix()) {
  ## Return an object with matrix and a matrix cache along with methods to manipulate these
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCached <- function(y) m <<- y
  getCached <- function() m
  list(set = set, get = get,
       setCached = setCached,
       getCached = getCached)
}


# cacheSolve
# Returns the inverse of the base matrix contained in an object created by makeCacheMatrix.
# If he inverse is cached then this is returned and a message output indicating this.
# If not cached then the inverse is calculated and stored in the cache.
# 
# Args:
#   x: an object created by makeCacheMatrix
# 
# Returns:
#   The inverse of the base matrix in x.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getCached()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setCached(inverse)
  inverse
}