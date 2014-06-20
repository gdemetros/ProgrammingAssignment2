## makeCacheMatrix creates a special matrix that can be 
## passed to cacheSolve.  makeCacheMatrix allows user to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse of the matrix
##    get the value of the inverse of the matrix
## cacheSolve takes special matrix created by makeCacheMatrix
## and calculates the inverse of that matrix.  But it first
## checks to see if the inverse has already been calculated.
## if so it returns the cached value.


## makeCacheMatrix takes an invertible matrix and returns
## a special matrix that can be passed to cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invMatrix <<- solve
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a special invertible matrix created by
## makeCacheMatrix and returns the inverse matrix of it
cacheSolve <- function(x, ...) {
   invMatrix <- x$getInverse()
   if(!is.null(invMatrix)) {
      message("getting cached data")
      return(invMatrix)
   }
   data <- x$get()
   invMatrix <- solve(data, ...)
   x$setInverse(invMatrix)
   ## Return a matrix that is the inverse of 'x'
   invMatrix

}
