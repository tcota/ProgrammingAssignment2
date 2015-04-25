## Assignment2: Caching the Inverse of a Matrix
## The function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse;
## Then the function cacheSolve: This function computes the inverse of 
## the matrix returned by makeCacheMatrix. 

## The function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse. The function is really a 
##list containing a function to: 
# 1. set the value of the matrix;
# 2. get the value of the matrix;
# 3. set the value of inverse of the matrix;
# 4. get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The function cacheSolve returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse and sets the value in the
## cache via setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
