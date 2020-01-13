## These function is designed to caching the inverse of a matrix.
## Caching is used because Matrix inversion is usually a costly computation
## and will potentially time-consuming computations.


## These function (makeCacheMatrix) creates a special "matrix" object 
## that can cache its inverse.
## Special "matrix" is actually a list containing function to
##       1. set the value of the matrix
##       2. get the value of the matrix
##       3. set the value of the matrix inversion
##       4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function () inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## These function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above
## if you try to cacheSolve on the matrix that is same with special
## "matrix", An informative message will be shown and
## result is returned instead.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


