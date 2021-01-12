## These functions compute the inverse of a matrix
## and cache it to save time

## makeCacheMatrix creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already
## been calculated then cacheSolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        i = x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
