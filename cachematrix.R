## Functions to solve the inverse of a matrix and cache the result.

## Creates a container that can hold a matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        xinv <<- NULL
        x <<- y
    }
    list(set = set, get = function() x,
         setinverse = function(y) xinv <<- y, getinverse = function() xinv)
}

## Solves and caches the inverse of the matrix, so that repeated calls return the cached result until the matrix is changed.

cacheSolve <- function(x, ...) {
    xinv <- x$getinverse()
    if (is.null(xinv)) {
        message("solving for the inverse")
        xinv <- solve(x$get(), ...)
        x$setinverse(xinv)
    } else {
        message("using cached solution")
    }
    xinv
}

