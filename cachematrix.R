## Thes two functions allow to create and smartly inverse matrices
## using cached values.

## Smart matrix object, carries a cahce of its own inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Solves for inverse of matrix, writes it into cache

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}