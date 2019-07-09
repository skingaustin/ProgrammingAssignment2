## This is a function that makes a matrix object that cantains the matrix 
## and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function calculates the inverse and caches it in the matrix object 
## that was created in the funtion above

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat, ...)
    x$setInverse(m)
    m
}
