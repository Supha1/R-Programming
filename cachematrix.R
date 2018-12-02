## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x                         ## get the matrix     
        setInverse <- function(solve) Inv <<- solve ## calculate the inverse
        getInverse <- function() Inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if (!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }                                          ## checks to see if the mean has already been calculated
        data <- x$get()
        Inv <- solve(data, ...)                    ## calculate the inverse
        x$setInverse(Inv)                          ## set the value of the inverse in the cache
        Inv
        ## Return a matrix that is the inverse of 'x'
}
