## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inverseMatrix <<- inverse
        getinverse <- function() inverseMatrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        theInverse = x$getinverse()
        if(!is.null(theInverse)){
                message("getting inverse matrix")
                return(theInverse)
        }
        matrix <- x$get()
        theInverse <- solve(matrix)
        x$setinverse(theInverse)
        theInverse
}
