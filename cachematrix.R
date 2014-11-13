## The function, makeCacheMatrix, creates a special invertable "matrix", through a list of functions that
## set the value of the matrix & get the value of the matrix
## set the value of the inverse matrix & get the value of the inverse matrix
## The function, cacheSolve, applies the 'solve' function to calculate the inverse of the special "matrix" created by makeCacheMatrix. 
## Before calculating, cacheSolve will first check if the inverse has already been calculted and gets it from the cache.
## Should it not already be calculated & cached, it will calculate the inverse of the matrix and sets it in the cache via the setinverse function.



## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## Should the inverse has already been calculated and there has been no change, cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}