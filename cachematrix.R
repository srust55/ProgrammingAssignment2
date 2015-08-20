## The functions below, when used together, allow one to return the inverse of a matrix
## while avoiding the inverse calculation if it has already been cached

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of a matrix returned by makeCacheMatrix
## If the inverse has already been calculated, it should be retrieved from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    message("calculating and caching the inverse")
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
