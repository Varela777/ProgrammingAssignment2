## Programming Assignment 2: Caching the Inverse of a matrix 
## As mentioned in the introduction, matrix inversion can be a time-consuming computation 
## As a result, instead of computing it repeatedly, we can instead cache the inverse of a matrix
## Below I've listed the functions I used to create a special object which stores the matrix
## and caches its inverse. 

## Creates a special matrix object that is capable of caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              #Initialisation of inv
                          #Create the matrix in the working environment)
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
    #Get the value of the matrix 
  get <- function() x
    #Invert the matrix and store in inv 
  setInverse <- function(inverse) inv <<- inverse
    #Get the inverted matrix from inv
  getInverse <- function() inv
  #Return the created functions to the working environment 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This next function determines the inverse of the special matrix created by the 
## makeCachematrix written above. If the matrix remains unchanged, and the inverse has already 
## been determined, then this function should retrive the inverse of the matrix from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()    #Try to get the matrix inverse stored in the cache
                           #If it exists in the cache, it is returned
                           #If not, then create the matrix in the working environment
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)           #Return matrix
  }
  data <- x$get()         #Creates matrix
  inv <- solve(data)      #Returns the inverse of the matrix
  x$setInverse(inv)       #Stores the inverted matrix in cache
  inv
}

