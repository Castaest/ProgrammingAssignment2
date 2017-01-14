## First function creates a special "matrix" object that can cache its inverse
## Output of the function is list of functions, which allows to handle matrix and its inverse.

## Like in the vector example function's output has four functions - two setters (for original matrix and inverse matrix)
## and two getters. Names of functions are: set and get - for original matrix, and to_invert and get_inverted - for inverted
## matrix.

## Please, take into consideration, that both functions do not have error handlers (e.g. checking whether argument matrix is squared one)
## because it wasn't noted in assignment

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    to_invert <- function(inv) inverted <<- inv
    get_inverted <- function() inverted
    list(set = set, get = get,
         to_invert = to_invert,
         get_inverted = get_inverted)
}


## Second function inverses matrix, stored in makeCacheMatrix object or, if it was already calculated, just returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverted()
    if(!is.null(m)) {
        message("getting cached inverted matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$to_invert(m)
    m
}
