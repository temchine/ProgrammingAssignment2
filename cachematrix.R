## Put comments here that give an overall description of what your
## functions do
## This script creates two functions that together allow the retrieval of
## previously inverted matrices, and in the event a matrix has not been
## run prior it will invert it and save the result.

## Write a short comment describing this function
## This script essentially creates a list named of four parts
## set contains the matrix
## get retrieves the matrix data
## setinverse calls function to invert the matrix
## getinvers retrieves the inverted matrix data

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## Write a short comment describing this function
## This function checks the previously created list to see if
## a matrix has been previously inverted and stored. If it has
## it returns the cached data with a message. If the matrix has
## not been inverted it performs the inversion and saves the 
## new matrix to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!isnull(m)) {
        message("getting cached data")
        return(m)
    }
    else {
        data <-x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
    }
}
