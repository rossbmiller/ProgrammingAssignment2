## These functions are identical in form to the examples provided for 
## Programming Assignment 2, with the obvious difference that the 
## "mean" function has been replaced with "solve."
## Some variable names were replaced for the sake of posterity, and the 
## class of the default input for makeCacheMatrix was changed. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
			s <- NULL
            set <- function(y) {
                    x <<- y
                    s <<- NULL
            }
            get <- function() x
            setinv <- function(solve) s <<- solve
            getinv <- function() s
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
            if(!is.null(s)) {
                    message("getting cached data")
                    return(s)
            }
            data <- x$get()
            s <- solve(data, ...)
            x$setinv(s)
            s
}

## For the sake of another commit. 


