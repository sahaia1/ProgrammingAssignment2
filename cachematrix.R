# The functions listed here cache time comsuming computation of 
# computing the inverse of a matrix so that the cached value can
# be used later.

## makeCacheMatrix function introduces four functions
## set - which caches the matrix and initializes the inverse
## to NULL
## get - returns the cached matrix
## setinv - sets the value of the inverse in the cache
## getinv - get the value of the cahced inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
 
}


## cacheSolve - A function which returns the inverse of a 
## given matrix x. It calculates the inverse and sets the value
## in the cache in case it is not cached already. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
