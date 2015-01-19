##  Pair of function that allow the inverse of a matrix to be calculated and
##  automatically cached. Example usage:
##    mat <- makeCacheMatrix(matrix(rnorm(16),4))
##    cacheSolve(mat) # calculates inverse
##    cacheSolve(mat) # uses cached result

## Creates a list containing functions to get and set a matrix and it's inverse
## Pass the object this function creates to cacheSolve to calculate the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate the inverse of a matrix created with the makeCacheMatrix function
## The inverse is cached in the global namespace the first time this function
## is called so that future call for the same matrix object are faster.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
