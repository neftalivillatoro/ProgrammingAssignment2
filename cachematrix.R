## In general, matrix inversion is a costly computation. But there are benefits to caching the inverse of a matrix
## instead of doing repeatedly. The next functions do exactly that.

## The next function does: set the value of matrix, get the value of the matrix, set the value of the inverse of the matrix
## and finally get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The next function returns a matrix that is the inverse of "X"... where X is invertible

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
