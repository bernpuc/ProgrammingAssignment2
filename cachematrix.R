## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions cache the inverse of a matrix.

## Function which creates a "class object" containing the matrix, the inverse of
## the matrix, functions to get and set class variables.

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
    set <- function(y) {
    	x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv_m <<- solve
    getinverse <- function() inv_m
    list(set = set,
    	 get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Calculate the inverse matrix of the matrix returned from makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv_m <- x$getinverse()
    
	if(!is.null(inv_m)) {
    	message("getting cached inverse matrix")
        return(inv_m)
    }
    # Calculate the inverse matrix and store in object
    data <- x$get()
    inv_m <- solve(data)
    x$setinverse(inv_m)
    inv_m
}
