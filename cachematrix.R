## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The pair of functions below cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) 
{
    t <- NULL
    set <- function(y) 
    {
        x <<- y
        t <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) t <<- solve
    getsolve <- function() t
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The following function calculates the inverse of the "special" matrix x.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips computation.
## Otherwise, it calculates the inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) 
{
    t <- x$getsolve()
    if(!is.null(t)) 
    {
        message("getting cached data")
        return(t)
    }
    data <- x$get()
    t <- solve(data, ...)
    x$setsolve(s)
    t
}
