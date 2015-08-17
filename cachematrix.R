# makeCacheMatrix returns a list of functions
# Stores a matrix and a cached value of the inverse 
# of the matrix. 
# Contains the following functions:
# sMatrix      
# gMatrix      
# cacheInverse   
# getInverse     

## returns a list of functions

makeCacheMatrix <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        sMatrix <- function(newValue) {
                x <<- newValue
                # flush the cache
                cache <<- NULL
        }
        
        # returns the stored matrix
        gMatrix <- function() {
                x
        }
        
        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        
        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(sMatrix = sMatrix, gMatrix = gMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## returns the inverse of matrix created with makeCacheMatrix

cacheSolve <- function(y, ...) {
        
        # get the cached value
        inverse <- y$getInverse()
        
        # if a cached value exists return it
        if(!is.null(inverse)) {
                
                message("getting cached data")
                
                return(inverse)
        }
        
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$gMatrix()
        
        inverse <- solve(data)
        
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
