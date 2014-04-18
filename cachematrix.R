## Put comments here that give an overall description of what your
## functions do

## Create a structure from a matrix that carries its own inverse, if set with setinverse().

makeCacheMatrix <- function(x = matrix()) {
        inverseX <- NULL
        set <- function(y) {
                x <<- y
                inverseX <<- NULL
        }
        get <- function() x
        setinverse <- function(theInverse) inverseX <<- theInverse
        getinverse <- function() inverseX
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Obtain inverse matrix of a CacheMatrix structure, or, if does not exist, 
##      then compute the inverse and store in the CacheMatrix structure.

cacheSolve <- function(x, ...) {
        inverseX <- x$getinverse()
        if(!is.null(inverseX)) {
                return(inverseX)
        }
        data <- x$get()
        theInverse <- solve(data, ...)
        x$setinverse(theInverse)
        theInverse
}
