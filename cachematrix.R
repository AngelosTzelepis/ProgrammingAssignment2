## "makeCacheMatrix" defines other functions to 
##      use for storing and retrieving matrix data.
## "cacheSolve" uses those functions to know
##      whether to return cached data or to perform
##      the required calculation itself



## The input x is assumed to be an invertible matrix
## Return a list of functions to the working environment
##      to set the matrix, get the matrix, set the inverse 
##      of the matrix, and get the inverse of the matrix
## This list of functions becomes the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    # initialize the empty cache
    cache <- NULL
    
    # set the matrix 
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # invert and cache the matrix
    setinv <- function(solve) cache <<- solve 
    
    # get the cached inverted data
    getinv <- function() cache
    
    # return the functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## x is the returned list of functions from makeCacheMatrix()
## This function returns the inverse of the original matrix
##      from cache if already calculated. If not, it calculates 
##      the inverse using "solve" and stores it in cache.

cacheSolve <- function(x, ...) {
    
    # Load inverted matrix data using getinv function
    # as defined above. May be empty.
    cache = x$getinv()
    
    # If inverse has already been calculated (is not null)
    # return that cached data and exit function
    if (!is.null(cache)){
        message("getting cached data")
        return(cache)
    }
    
    # If not, invert the matrix 
    matrix = x$get()
    cache = solve(matrix, ...)
    
    # Caches the cache matrix with setinv function
    x$setinv(cache)
    
    return(cache)
}