## Caching the Inverse of a Matrix
## Assume that the matrix supplied is always invertible.
## In the real world you may meet with  - "Error in solve.default(data, ...) : 
## Lapack routine dgesv: system is exactly singular:Lapack routine dgesv:  ...
## system is exactly singular"
## Just try another matrix (with det<>0)

## makeCacheMatrix creates a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve technically makes inverse or gets cashed data

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
## Return a matrix that is the inverse of 'x' or calculate and return
}
