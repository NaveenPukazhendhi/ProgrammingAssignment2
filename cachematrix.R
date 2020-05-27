## These functions are intended to cache an inverse of a matrix


## This particular function creates an special object to cache the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m
}
