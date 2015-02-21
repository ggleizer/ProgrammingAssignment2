## cacheMatrix.R is a set of functions that implement a matrix
## type with cached inverse. The inverse will be only computed
## if it hasn't before. Otherwise, it is retreived from the
## memory.

## makeCacheMatrix builds a list of sets and gets to manipulate
## the cacheMatrix and its inverse. Its functions are set, get,
## setinv and getinv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  ## if matrix is changed, return inverse to NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve receives as input a cacheMatrix and checks if 
## the inverse exist. If it doesn't, it calculates it through
## solve() function and stores it in the cacheMatrix. If it
## exists, it simply gets the cached inverse and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
