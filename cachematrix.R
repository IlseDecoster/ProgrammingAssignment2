## With makeCacheMatrix, a special matrix is returned, which can be passed
## to the cacheSolve function and where, either the inverse is cached and
## returned, either the inverse is calculated.

## This function creates a special "matrix" object which is in fact
## a list containing 4 functions, to get and set the value of the 
## matrix passed to the function, and to get and set the inverse of
## that matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function () i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" that
## is passed to it and that was created by the makeCacheMatrix function.
## The function will either cache the inverse, say it was chaced and 
## then return it, either it will calculate the inverse and return the
## calcaulted matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
