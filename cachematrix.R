## fork from https://github.com/rdpeng/ProgrammingAssignment2  
## improve performance of computing the inverse of matrix by caching result 

## prepare the function for cacheSolve() 

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setRmatrix <- function(rmatrix) r <<- rmatrix
    getRmatrix <- function() r
    list(set = set, get = get,
         setRmatrix = setRmatrix,
         getRmatrix = getRmatrix)
}


## check if the result is ready, if the result is ready, return the cached result directly 

cacheSolve <- function(x, ...) {
    r <- x$getRmatrix()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setRmatrix(r)
    r
}
