## This code contains two functions to collectively compute the inverse of 
## given matrix, assuming that the provided matrices are invertible

## The following function requires a matrix input and provides a result of a list
## containing set, get, setInverse and getInverse functions. The get and getInverse retrieve the 
## matrix and its inverse, whereas the set and setInverse allows the user to change the matrix and/or
## inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(matrixInverse) m <<- matrixInverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The following function returns the inverse of the matrix after user inputs it into the above function
## If the inverse has already been cached, it will be retrieved from the cache, and not calculated again.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m ## Returns the matrix, m that is the inverse of 'x'
}
