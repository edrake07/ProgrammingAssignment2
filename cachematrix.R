##function makeCacheMatrix has one argument which must be a square matrix
##4 functions are created: get(), set(), setinv(), getinv()
##note that cached inverse must be set by user
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {    ##creates set function which will input new matrix into makeCacheMatrix   
                x <<- y         ##overwrites original matrix with new input
                inv <<- NULL    ##clears cached inverse
        }
        get <- function() x     ##creates get function which will print matrix
        setinv <- function(inverse) inv <<- inverse    ##setinv function will manually cache inverse
        getinv <- function() inv    ##getinv will print inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##cacheSolve will first try to pull the cached inverse from makeCacheMatrix
##if no inverse is cached, the inverse will be calculated
##message will print if cached inverse if used
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        inv ##prints inverse of x
}