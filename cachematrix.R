## makeCacheMatrix and cacheSolve work together to help make computing the inverse of a
## matrix more efficiently by caching data
## assumption: the matrix is always invertible

## makeCacheMatrix takes a square invertible matrix as an argument and returns a list
## that contains 4 funtions: (1) set the value of the matrix and its inverse to NULL
## (2) get the value of the matrix (3) set the value of its inverse in cache 
## (4) get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    # the set function caches the matrix and sets the inverse of the matrix to NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    # get function returns the matrix
    get <- function() {
        x
    }
    # setInvMatrix sets the inverse of the matrix in cache
    setInvMatrix <- function(im) {
        invMatrix <<- im
    }
    # getInvMatrix returns the inverse of the matrix
    getInvMatrix <- function() {
        invMatrix
    }
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}

## cacheSolve takes a square invertible matrix (and optional arguments for solve) and returns the inverse
## the function returns cached data if the inverse exists in cache otherwise it computes the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInvMatrix()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    } else {
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInvMatrix(invMatrix)
        invMatrix
    }
}
