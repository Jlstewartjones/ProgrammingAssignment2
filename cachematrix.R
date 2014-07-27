## This function gets the value of a matrix, calculates the 
## inverse of that matrix, and caches the inverse for future use

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve ##get the inverse of the matrix
    getinv <- function() inv ## cache the inverse for future use
    list(set = set, get = get, ##creates single list with all information
         setinv = setinv,
         getinv = getinv)
}


## This function checks to see if the inverse of the matrix 
## has already been calculated. If it has, it will get the result
## from the cache. If it has not, it will calculate the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) { ##Check to see if inverse has already been calculated
        message("getting cached data") ##if inverse has already been calculated, get answer from cache
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) ##if no inverse in cache, calculate inverse
    x$setinv(inv)
    inv
}
