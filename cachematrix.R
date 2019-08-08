## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below is a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        opt <- NULL
        set <- function(y) {
                x <<- y
                opt <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) opt <<- inverse
        getInverse <- function() opt
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        opt <- x$getInverse()
        if (!is.null(opt)) {
                message("getting cached data")
                return(opt)
        }
        mat <- x$get()
        opt <- solve(mat, ...)
        x$setInverse(opt)
        opt
}
