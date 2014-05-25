## Excercise to understand implement caching values of potentially expensive
## operations. e.g. Computing inverse of a matrix

## Defines a special "MATRIX" object that stores routines to set and retrieve 
##it along with that of its its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # defines 4 fuctions to get and set matrix and its inverse
        set <- function(m) {
                x <<- m
                inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        
        #defines a special matrix object
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of square matrix 'x'

cacheSolve <- function(x, ...) {
        #retrieves inverse of matrix. If NULL then it is calulated. If not it is retrieved from cache
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Getting cached result")
                return(inv)
        } else {
                inv <- solve(x$get())
                x$setinverse(inv)
                return(inv)
        }
}
