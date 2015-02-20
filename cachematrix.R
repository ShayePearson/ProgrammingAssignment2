## The following is a a pair of functions that cache
## the inverse of a matrix.

## The first function, makeCacheMatrix creates a list
## containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of inverse of the matrix 
## 4. get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) minv <<- inverse
        getmatrix <- function() minv
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


## This function returns the inverse of a matrix. 
## HOwever,if the inverse has already been calculated, 
## it gets the inverse from the cached and skips the computation.
## If the inverse has not been previously calculated,
## it calculates the inverse of the matrix and sets the inverse
## to the cache with setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getmatrix()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setmatrix(minv)
        minv
}