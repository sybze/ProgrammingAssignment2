## The two functions work together and calculate the inverse to the initially entered matrix
## and store it in the cache. The inverse is then retrieved instead of being calculated anew
## when the two functions are executed again for the same matrix.

## Setter and getter functions for matrix and inverse.

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


## Output list of the above function is entered; if cached inverse value is not NULL
## cached value is retrieved, otherwise inverse is computed, assigned to i and cached
## to be retrieved next time.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
