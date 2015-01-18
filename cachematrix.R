## The below functions are used to get the inverse of a matrix.
## To speed up the process, the inverse is obtained from a cache (if stored in it). 
## If the cache is empty, the inverse is calculated & stored in cache for subsequent retrievals.


## makeCacheMatrix:makecacheMatrix creates a special vector by taking a matrix as input.
## This vector can be used to set, reset the matrix and cahce the matrix inverse.
## It consists of the following functions:
## 1. set - set the matrix to another value based on input (y)
## 2. get - get the matrix
## 3. setinv - cache the matrix inverse
## 4. getinv - get the matrix inverse stored in the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
        x_inv <- NULL      ##x_inv is to chache the inverse of the matrix
        set <- function(y) {
            x <<- y
            x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) x_inv <<- inv
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve produces the inverse of the matrix either by getting it from the cache or calculating it using the solve
## The input matrix is first checked to see it already has a cached inverse. If not, the inverse is calculated and stored in the vector
cacheSolve <- function(x, ...) {
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve (data, ...)
    x$setinv(x_inv)
    x_inv
}
