## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function "makeCacheMatrix" creates a matrix and
## returns a list of functions used by the "cacheSolve"
## function to get or set the invertion of that matrix 
## in cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Write a short comment describing this function

## The function "cacheSolve" calculates the inverse of
## the matrix created by the function "makeCacheMatrix"
## If inverted matrix does not exist in the cache it
## is created in the working environment and the inversion
## of that matrix is stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

## Additional code that creates random matrix "x" and 
## runs a test to see if the matrix can be inverted, a
## requirement for the above functions to work.

x <- matrix(rnorm(4),2,2)
unlist(determinant(x))
det(x) -> y
if (y < 0) {stop(print("matrix cannot be inverted"))}
if (y >= 0) {print("matrix can be inverted")}
class(x)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
cacheSolve(x1)
