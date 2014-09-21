## This R file contains functions to cache the inverse of a square matrix.  
## When called, the function will return the inverse of the supplied matrix.  If
## the matrix is the same as the last time the function was called, it will simply
## return the cached value from the previous call.  If, however, the matrix is not
## the same, the inverse will be recalculated using the new matrix, the value will
## be cached, and the new inverse will be returned.

## The function makeCacheMatrix will accept an arguement containing a square matrix.
## It will then calculate and return the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                             ## initialize the inverse variable
    
    set <- function(y) {                        ## this function will store the 
        x <<- y                                 ## value of the matrix and its
        inverse <<- NULL                        ## inverse in the parent environment
    }
  
    get <- function() x                         ## This retieves the value of x
  
    setInverse <- function(solution) {          ## This function stores the inverse
        inverse <<- solution                    ## solution calculated for x
    }
  
    getInverse <- function() inverse            ## This function returns the inverse
  
    ## This ruturns a list containing all defined functions so that they can be used
    ## to store and access the cached values.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function accepts an arguement a list returned by the 
## makeCacheMatrix function and returns the inverse of the contained 
## square matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()                         ## Retrieve the value of the inverse
    if(!is.null(m)) {                           ## if the value is not null return
        message("Getting cached data...")       ## the cached value
        return(m)
    }
    
    data <- x$get()                             ## calculate a new inverse of the 
    m <- solve(data)                            ## supplied square matrix, cache 
    x$setInverse(m)                             ## the value, and return 
    m                                           ## the solution
}
