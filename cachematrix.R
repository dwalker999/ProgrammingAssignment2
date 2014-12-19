##  The two functions in this source file accept a matrix, then
##  solve the matrix for the inverse.
##  

## This function caches a matrix entered as a parameter
## and returns a list object.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function accepts a list created from the makeCacheMatrix function
## and either calculates, caches, and returns the solved matrix, or retrieves
## and returns the previously cached solved matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
                x            
        }
        else
                message("writing matrix to cache")
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}