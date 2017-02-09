## The purpose of these functions is to cache the inverse of a
## matrix, since matrix inversion can be a costly computation.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special matrix
## returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
        
## Define a square invertible matrix as T
        
# T = matrix(c(4,3,2,1),nrow=2,ncol=2,byrow=TRUE)
        
## Return inverse of the square matrix
                
# Test <- makeCacheMatrix(T)
                
# cacheSolve(Test)
                
## Run cacheSolve(Test) again to see message, "getting cached data"

