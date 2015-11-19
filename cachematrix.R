## makeCacheMatrix function creates a special matrix object than can cache its 
## inverse.
##
## cacheSolve function computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix)
## has not changed), then the cachesolve should retrieve the inverse from the
## cache.


## This function creates a matrix. It contains four functions: set, get,
## setsolve and getsolve. List() stores the 4 functions in the function
## makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## This function first verifies if value m (inverse of the matrix) is cached and
## is not NULL. If not, it calculates the inverse (m) and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

