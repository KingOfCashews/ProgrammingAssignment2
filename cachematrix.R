## The functions below can be used to store a matrix object and its inverse,
## either by explicitly calculating for a new matrix, or by fetching a
## cached version of the previously calculated inverse.


## This function takes a matrix object as an argument, setting and getting
## the matrix and its inverse in a list object.  By default, if no argument
## is given, an empty matrix is assumed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a matrix object from makeCacheMatrix as an argument and 
## either calculates or fetches the cache value of the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) %*% data
  x$setinverse(i)
  i
}
