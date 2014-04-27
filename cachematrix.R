##This code efficiently computes the inverse of a matrix by fetching it from cache if already present
## Else it computes the value

## Fetches the inverse of a matrix if present in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(iMatrix) inverseMatrix <<- iMatrix
        getInverse <- function() inverseMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of a matrix if its present in the cache or else computes the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$getMatrix()
        inverseMatrix <- data %*% solve(data)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
