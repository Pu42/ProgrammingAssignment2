## The following functions can be used to cache potentially time-consuming inversions
## matrices. Thus, when a matrix is inverted, the result is cached, so that, 
## when needed again, the result can be looked up in the cache rather than recomputed.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It is a list containing a funtion to
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse of the matrix
##  4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the cache value
    m <- NULL
    
    # set the value of the matrix and reset the inverse
    set <- function(y) {
        x <<- y
        m <<- Null
    }
    
    # get value of the matrix
    get <- function() x
    
    # set value of the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    
    # get value of the inverse of the matrix
    getinverse <- function() m
    
    # return a list of all used functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse of the matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
    
    # get the cached inverse of the matrix if existent
    m <- x$getinverse()
    
    # return cached inverse if it exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if no cached value exists, get matrix
    data <- x$get()
    
    ## compute inverse of the matrix
    m <- solve(data)
    
    ## cache the inverse
    x$setinverse(m)
    
    ## return the value of the inverse of the matrix
    m
}
