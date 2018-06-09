## makeCacheMatrix() stores a matrix and its inverse.
## cacheSolve() takes a makeCacheMatrix() object as an input
## and - if it exists - will retrieve the matrix inverse from the cached value that is stored 
## in the makeCacheMatrix() object's environment. If no inverse exists, the inverse is calculated and cached.

## makeCacheMatrix() - takes a matrix as an input and 
## returns a list object containing four functions
## to set or retrieve the input matrix and its inverse

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

## cacheSolve - takes a makeCacheMatrix() object as an input and returns
## a cached inverse - if it exists - or calculates the inverse otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
