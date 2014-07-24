## This function is intended to save time when calculating the inverse of a matrix.

## The following function is to create a matrix that can cache its inverse. The matrix must be 
## invertable. The operators <<- tell R to look for that variable in a parent function and not to 
## create a new variable with that name.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y 
                        m <<- NULL 
                }
                get <- function() x
                setinversematrix <- function(solve) m <<- solve 
                getinversematrix <- function() m
                list(set = set, get = get,
                     setinversematrix = setinversematrix,
                     getinversematrix = getinversematrix)
        }



## This function will allow you call the cached inverse matrix. This will greatly decrease the time 
## it takes to subsequently calculate the inverse matrix. The "solve" function calculates the 
## inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}

