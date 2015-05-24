## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly. Here is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverseCached <- NULL
        set <- function(y) {                           # set the value of the vector
                x <<- y
                inverseCached <<- NULL
        }
        get <- function() {                            # get the value of the vector
                x
        }
        setInverse <- function(invertedMatrix) {       # set the value of inverted matrix
                inverseCached <<- invertedMatrix
        }
        getInverse <- function() {                     # get the value of the inverse
                inverseCached
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {                               # if the mean was cached - 
                message("getting cached data")
                return(m)                               # exit program without excuting subsequent code
        }
        data <- x$get()                                 # otherwise, put the data in 'data'
        m <- solve(data, ...)                           # compute the inverse of the data
        x$setInvesre(m)                                 # call function to cache the inverse
        m                                               # return the inverse
}