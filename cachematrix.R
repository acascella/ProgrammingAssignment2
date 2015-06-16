## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
# make a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                 x <<- y
                 inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
}


## Write a short comment describing this function
#returns inverse of x if not yet cached, otherwise calculates and
#caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv))  {
                message("getting cached data")
                return(inv)
        }
        
        inv <- solve(x$get())
        x$setInverse(inv)
        inv
        
}
