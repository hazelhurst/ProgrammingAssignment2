## These two functions together cache the inverse of a matrix

## makeCacheMatrix takes a normal matrix object and returns a new "matrix" object
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x  
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve takes ac "matrix" object (created by the makeCacheMatrix function)
# as an arugment and returns the inverse of that "matrix" object. If the inverse
# has already been computed,cacheSolve will not compute the inverse, but instead
# return the cached value of the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
