# inverse of a matrix using solve()

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
