## Matrix inversion is usually a costly computation and there is benefit 
## to caching the inverse of a matrix rather than having to compute it 
##repeatedly.

## My functions cache the inverse of a matrix

## This function creates a special object that stores 
## a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverese in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x ["getinverse"]
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x ["get"]
        i <- solve(data, ...)
        x ["getinverse"] (i)
        i
}

