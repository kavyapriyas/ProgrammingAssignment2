

## This function creates a special "matrix" object that can cache its inverse.
## This is a part of Assignment Week 3 - 8/14/2016


makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) matInv <<- inverse  
  getinverse <- function() matInv     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getinverse()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setinverse(matInv)
  matInv
}

## This is to test the function
m <- matrix(1:4,2,2)
m
cacheSolve(makeCacheMatrix(m))