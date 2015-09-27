## makeCacheMatrix is a function that creates a matrix
## with the added functionality of caching the inverse.
## Using the cacheSolve function on the special matrix 
## calculates the inverse only when the inverse has not 
## already been cached, or if the matrix has changed 
## since the last caching.

## The following function takes a matrix to make as a special 
## matrix with matrix inverse caching functionality.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL #reset the cached inverse to NULL when the matrix is changed
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function interfaces with the special matrix 
## created with makeCacheMatrix to cache the inverse (if needed)
## and return the inverse.

cacheSolve <- function(x, ...) {
    #check if the inverse has already been cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("retrieving cached data")
        return(inv)
    }
    #solve and cache the inverse if it hasn't been cached
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
