## These methods let the user create CacheMatrices, a data structure useful for
## computing inverses in a time-efficient way via caching.
## 
## I based this implementation heavily on the example of caching the computation
## of the mean of a vector, since they seemed to be basically the same.

## Takes a matrix and returns a list of four functions:
## 1. get(), which returns the underlying matrix
## 2. set(mat), which sets the underlying matrix to a new value, and clears the
##        now-outdated cached inverse
## 3. getsolve(), which returns the cached inverse matrix
## 4. setsolve(sol), which sets the cached inverse to a new solution matrix
## This must only be called for invertible matrices.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solution) inv <<- solution
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Takes a structure outputted by makeCacheMatrix and outputs the inverse of the
## underlying matrix. First checks if there's a cached inverse, and returns that
## if it finds one. Otherwise, it will expend the time to compute the underlying
## matrix's inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("Found cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
