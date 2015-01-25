## Matrix inversion is usually a costly computation and there is benefit 
## to caching the inverse of a matrix rather than having to compute it 
##repeatedly.

## My functions cache the inverse of a matrix

## This function creates a special object that stores 
## a matrix and caches its inverse. You will need to create a new variable with 
## this function.

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

## It is important to reiterate that this function will be used to create a 
## variable that then will be passed to the the function below.
## So, if the matrix used to test this function is "mymatrix" then you would 
## create a variable called "x" as in x <- makeCacheMatrix(mymatrix)
## Then you would use the function below with "x" as in cacheSolve(x), rather 
## trying to do cacheSolve(mymatrix).

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the 
## setinverse function.
## the first time you run the code cacheSolve(x) - x being the variable that you
## created with the function above - you will get the calculated inverse of the 
## function but the second time you run cacheSolve(x) you will see the message 
## "getting cached data" which confirms the function works.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}       	 
