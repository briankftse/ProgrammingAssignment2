## makeCacheMatrix and cacheSolve are functions that enable the inverse of a
## matrix to be computed and cached.  makeCacheMatrix creates the getters
## and setters for our matrix, and cacheSolve computes the inverse of the
## matrix.


## makeCacheMatrix takes in a matrix and creates a list to perform 4 functions:
## 1. set(y) sets the matrix and resets the inverse of the matrix
## 2. get() returns the matrix
## 3. setinverse(inverse) caches the inversed matrix
## 4. getinverse() returns the cached inversed matrix

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


## cacheSolve returns the inverse of the special matrix.  If the inverse of the 
## matrix has already been calculated and cached, simply return the cached
## value.  Otherwise, compute the inverse and store the result in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
