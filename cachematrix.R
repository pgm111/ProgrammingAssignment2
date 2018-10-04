## https://github.com/rdpeng/ProgrammingAssignment2

## This assignment is to write two functions that:
## (1) create a special matrix, and 
## (2) cache the inverse of the special matrix

## makeCacheMatrix is a function that creates a special 
## "matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## CacheSolve is a function that computes the inverse of
## the special "matrix" returned by the MakeCacheMatrix function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}

## Testing the functions
testMatrix1 <- matrix(rnorm(16),4,4)
testMatrix1
testMatrix2 <- makeCacheMatrix(testMatrix1)
cacheSolve(testMatrix2)