## These functions create the inverse of a matrix and then cache it.
## When they are called to create an inverse of a matrix, they first 
## check the cache to see if it already exists, and if it does, simply
## return that value. Otherwise the inverse is created.


## This function creates an inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the inverse of the matrix already
## exists (if it was created by the previous function). If it does
## already exist, then it returns it. If it doesn't already exist,
## then it runs the previous function to create it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
