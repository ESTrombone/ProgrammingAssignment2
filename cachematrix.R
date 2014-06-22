##The following is script to a function that makes, then inverts a matrix

## This first function creates a special matrix, then assigns/retrieves values
## of matrix, then assigns/retrieves values of inverted matrix



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

## The cacheSolve function checks whether or not the inverted matrix 
## has already been cached. If it has, then it returns the cached values. 
## Otherwise, it calculates the inverse matrix and prints it. 


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}