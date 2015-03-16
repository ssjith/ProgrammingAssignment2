## Computing the inverse of a square matrix(invertible) and 
## cache for future

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the values of the matrix
## get the values of the matrix
## set inverse of the matrix
## get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the Inverse of the special "matrix"
## created with the makeCacheMatrix function. 
## First checks to see if the Inverse has already been calculated. 
## If so, it `get`s the Inverse Matrix from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the matrix 
## and sets the value of the Inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
