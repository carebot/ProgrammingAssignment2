## Utility functions to allow caching of solving the inverse of a matrix.
## A special matrix stores the inverted result of itself so subsequent
## calls to a cache enabled solve function can return the saved result of a prior computation.


## an enhanced matrix function that stores both the matrix and the inverted
## result of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(input) inverse <<- input
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## solve function that takes a special cache matrix and checks for an existing
## value before computing the inverse of the matrix
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    ## try to solve, only cache result if successful
    try(
        {
            ## do not allow extra params, so the inverse will be calculated
            inverse <- solve(data)
            x$setinverse(inverse)
            message("calculating inverse")
            return(inverse)
        }
    )
}
