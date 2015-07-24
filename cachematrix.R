## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(n) {
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
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
