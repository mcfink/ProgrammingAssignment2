## Here are two functions that attempt to minimize matrix inversion by storing
## a matrix's inverse once it has been computed once.  Future requests for that
## matrix's inverse will return the cached copy

## This function has a setter and getter function for both the input matrix (set, get)
## and the matrix's inverse, if it has been computed (setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(inverse){
        m <<- inverse
    }
    getinverse <- function(){
        m
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function returns the inverse of the input matrix.
## if the inverse has already been computed, the cached inverse is returned
## otherwise, this function computes the input matrix's inverse.

cacheSolve <- function(x, ...) {
    matrix <- x$getinverse()
    if(!is.null(matrix)){
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$setinverse(matrix)
    matrix
}
