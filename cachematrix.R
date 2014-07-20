## These functions can be used to cache the inverse of a matrix. 
## This is useful because matrix inversion is usually a costly computation. 

## This function creates a special "matrix" object that contains functions to 
## 1. set the value of a matrix; 2. get the value of a matrix; 
## 3. set the value of the inverse of a matrix; 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then this retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        i
}