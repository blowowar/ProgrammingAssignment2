# inverse of a matrix using solve()
# The following code is used to create a special matrix that stores a matrix and caches its inverse.

makematrixinv <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrixinv <- function(solve) m <<- solve
    getmatrixinv <- function() m
    list(set = set, get = get,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv)
}

# If X is a square invertible matrix, then the solve(x) function should return its inverse.
# If the inverse has already been calculated, then the CacheSolve function should retrieve the inverse from the cache.

cachematrixinv <- function(x, ...){
    m <- x$getmatrixinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrixinv(m)
    m
}
