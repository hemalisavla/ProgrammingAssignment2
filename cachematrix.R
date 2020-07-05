## matrix inversion required repeated computation whenever used. Caching the 
## inverse of a  matrix is both time- and cost-saving. Two functions have been 
## developed for this purpose - makeCaheMatrix and cacheSolve

## makeCacheMatrix() creates a special matrix object that can cache 
## its own inverse

makeCacheMatrix <- function (x = matrix()) {
  inverse <- NULL               # Initializes the cached inverted matrix
  set <- function(x) {          # Sets the object's matrix
    m <<- x
    inverse <<- NULL
  }
  get <- function() m           # Get the object's matrix
  setCacheMatrix <- function(solve) inverse <<- solve   # Sets inverted matrix 
  # into cache.
  getCacheMatrix <- function() inverse                  # Gets inverted matrix 
  # from  cache
  list(set=set, get=get, setCacheMatrix=setCacheMatrix, 
       getCacheMatrix=getCacheMatrix)                   # List of functions 
  # in makeCacheMatrix()
}

## The cacheSolve() function computes the inverse of the matrix created using 
## makeCacheMatrix function. It retrieves the inverse of a matrix from the 
## cache, instead of the need to recalculate it. If, the inverse matrix does not
## exist, it calculates the inverse, caches it and returns the inverted matrix 
## with the help of setCacheMatrix() function. 

cacheSolve <- function(x, ...) {
  inverse <- x$getCacheMatrix()            # Checks for cached inverted matrix
  if(!is.null(inverse)) {
    message("Getting cached inverted matrix")# If-then function to print message 
    return(inverse)                          # and return inverted matrix,
  }                                          #if available
  
  inputmatrix <- x$get()                     # If not available, get original 
  inverse <- solve(inputmatrix, ...)         # matrix, inverse it, set it into
  x$setCacheMatrix(inverse)                  # cache and return inverted matrix
  inverse
}