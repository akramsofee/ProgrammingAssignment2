## Together, the two functions seek to accelerate calculation of the inverse of a matrix
## This is done by caching results
## If results have not been cached, calculation is performed
## If results have been cached, calculation avoided, saving time

## makeCacheMatrix returns functions to set/get matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve uses list created in makeCacheMatrix as input

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    ## retrieves cache if inverse has been calculated 
    message("load cached data")
    return(inv)
  }
  ## calculates inverse using solve() function if otherwise
  matriks.data = x$get()
  inv = solve(matriks.data, ...)
  x$setinv(inv)
  return(inv)
}