## These functions imporve the performance of working with the inverse
## of a matrix by creating a matrix object that can cache the inverse
## and by providing a function that returns the cache of the
## inverse of matrix if availble and calculates it if the inveerse 
## is not cached.


## This is a matrix holder object that holds the matrix and the 
## cached inverse of the matrix.  It includes get and set 
## functions for accessing the matrix.  It also includes a 
## getInverse and setInverse method for setting and accessing 
## the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes an instance of makeCacheMatrix.  If the 
## instance of makeCacheMatrix already has the inverse of the 
## matrix caculated and cached, then it returns that cached value.  If 
## the inverse is not cached, then this function calculates the inverse and
## caches the result in the makeCacheMatrix instance for future requests.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}