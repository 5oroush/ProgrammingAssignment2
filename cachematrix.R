## The following pair of functions are used to create a special  
## "matrix" object that can compute and cache its inverse.

## Creates a special "matrix" object that can cache its inverse.
## Assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Matrix inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Computes the inverse of the special "matrix" object returned by makeCacheMatrix function.
## If the inverse has already been calculated then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x = makeCacheMatrix(), ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInv(inv)
        inv
}
