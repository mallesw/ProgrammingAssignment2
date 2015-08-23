## Cache the inverse of a matrix
## Time consuming computations such as matrix inversion benefit by caching
## the inverse of the matrix rather than computing repeatedly
## The following 2 functions stores a matrix and caches its inverse


## Creates a matrix object that can cache its inverse
## by setting the value of the matrix, getting the value of matrix
## setting the inverse of the matrix, and getting the inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Compute the inverse of the matrix created by makeCacheMatrix
## If inverse is already calculated, then the cached inverse is retreived.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setInverse(inv)
        inv
}
