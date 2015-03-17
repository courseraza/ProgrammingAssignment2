## Solving a matrix (getting its inverse) is an expensive operation, so these functions cache
## the results so that if the matrix doesn't change the cached result will be returned on the
## second call.
## Usage:
## m <- matrix(c(2,2,3,2),2,2)
## mm <- makeCacheMatrix(m)
## cacheSolve(mm)

## makeCacheMatrix takes the matrix and returns an object from the matrix that is cacheable.
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


## cacheSolve accepts an object from makeCacheMatrix, solves it, and caches the result.
## On the second call the cached result will be returned.
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
