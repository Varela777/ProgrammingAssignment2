## Programming Assignment 2: Caching the Inverse of a matrix 
## As mentioned in the introduction, matrix inversion can be a time-consuming computation 
## As a result, instead of computing it repeatedly, we can instead cache the inverse of a matrix
## Below I've listed the functions I used to create a special object which stores the matrix
## and caches its inverse. 

## Creates a special matrix object that is capable of caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This next function determines the inverse of the special matrix created by the 
## makeCachematrix written above. If the matrix remains unchanged, and the inverse has already 
## been determined, then this function should retrive the inverse of the matrix from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

