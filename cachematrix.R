## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve() retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      	 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


## Some tests
## B = matrix(c(1, 0, 2, 1, 2, 5, 1, 5, -1),nrow=3,ncol=3)
## C = makeCacheMatrix(B)
## cacheSolve(B)
## cacheSolve(B)
