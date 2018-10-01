##These functions are a pair, the first creating the cached matrix, the second
## returning it.


## This function creates a variable in which the inverse matrix is cached

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL              ##creates empty vector to be filled
        set <- function(y) {     ##sets values in the parent enviornment
                x <<- y
                i <<- NULL
        }
        get <- function () x      ##Def
        setinv <- function(invertmatrix) i <<- invertmatrix
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the matrix inverse of the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) { ##check to see if cached or not
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
