## makeCacheMatrix function

## This function creates a special "matrix" object that can cache its inverse
## The output is a list containing a function to 
## set the value of the matrix, get the value of the matrix,
## set the inverse of the matrix, get the inverse of the matrix
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL ## i represents the inverse of a matrix
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  ## get the inverse of the matrix
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  I <- makeCacheMatrix(x)
  ## First check if the inverse has already been computed. 
  i <- I$getinverse()
  ## If so, get the inverse from the cache and skip the computation.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If not, compute the inverse of the data and ...
  data <- I$get()
  i <- solve(data, ...)
  ## ... set the value of the inverse in the cache.
  I$setinverse(i)
  i
}

