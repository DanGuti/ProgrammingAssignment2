## makeCacheMatrix - Generates a matrix that can cache it's inverse.
##  get <- returns value of matrix
##  setinv <- saves the inverse to cache
##  getinv <- returns inverse value from cache
## cacheSolve - If not in cache, solves matrix and stores inv. in cache

## makeCacheMatrix: Generate a matrix, set it's value and allocate inverse var

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve - Returns matrix inverse from cache, or calculates and saves it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv     ## Return a matrix that is the inverse of 'x'

}

## DanGuti - 2014