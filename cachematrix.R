## Creates two vectors to compute the inverse of a matrix. Once the inverse
## is calculated, it is fetched from cache rather than re-calculated again,
## saving computing time.
## First, feed your matrix to the makeCacheMatrix function, and then pass it
## to CacheSolve to get the inverse.
## Example:
# > x <- matrix(c(2,2,3,2), nrow=2)
# > m <- makeCacheMatrix(x)
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(m)
# getting inverse from cache
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0



## Returns a list of functions to set and get a matrix (set, get)
## as well as its inverse (setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function(inverse) {
        i
    }
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes a makeCacheMatrix object as input and returns an inverse
## of its matrix. If it has been calculated before, it returns
## the value from cache.
## Warning: The matrix needs to be invertible, else an error is thrown.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting inverse from cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}