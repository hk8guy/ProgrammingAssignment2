## cachematrix.R helps calculate the inverse of matrix
## it caches the inverse for re-use
## it contains two function makeCacheMatrix(x) and cacheSolve(x)

## makeCacheMatrix(x) create a special "matrix" object that can cache the inverse of 'x'
## It creates a list of four functions: set(), get(), setinverse(), getinverse()
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                     # clear the inverse whenever the function is called
    set <- function(y) {
        x <<- y                         # replace the old matrix 'x' with new matrix input 'y' in containing environment
        inverse <<- NULL                # clear the inverse in containing environment
    }
    get <- function() x                 # return the matrix
    setinverse <- function(uservalue) 
        inverse <<- uservalue           # bypass the calculate and set user's "inverse" value
    getinverse <- function() inverse    # return the inverse stored
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(x) looks for the inverse by passing special "matrix" object 'x'
## in case the inverse has been calculated, it would get the result from cache
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {             # checked if the inverse has been calculated already
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)               # cache the inverse
    inverse    
}
