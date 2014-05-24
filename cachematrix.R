## This program will compute the inverse a matrix and store in the value in cache
## It will first check to see if the inverse has already been stored in cache

## makeCacheMatrix creates a list containing the inverse of the matrix
## and stores it in memory

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y  ## Create matrix and store in memory
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve checks to see if the inverse of the matrix has already been
## computed and stored. If it has not, it will compute the inverse
## of the matrix and store it in cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")  ## Check if inverse has been stored
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)         ## Return a matrix that is the inverse of 'x'
  x$setinverse(i)
  i
}
