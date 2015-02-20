## These functions allow a better performance when calculating the inverse matrix
## This is achieved making use of a cache, in order to avoid calculating again the 
## inverse matrix in case the original one is not modified

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the cached inverse matrix
        inverseMatrix <- NULL
        
        ## Function that sets the matrix and initializes the cached inverse matrix
        set <- function(y) {
                
                ## Check whether the matrix has been changed. In case it is not
                ## avoid initializing the cache
                changed <- TRUE
                if(all(dim(y)==dim(x))){
                        if(all(y==x)){
                               changed <- FALSE 
                        }
                }
                
                ## Apply new matrix value and initialize cache iff it has been changed
                if(changed){
                        x <<- y
                        inverseMatrix <<- NULL
                }else{
                        message("The matrix has not really been changed!")
                }
        }
        
        ## Functions that gets the actual matrix
        get <- function() x
        
        ## Function that sets the cached inverse matrix
        setinverse <- function(inverse) inverseMatrix <<- inverse
        
        ## Function that gets the cached inverse matrix
        getinverse <- function() inverseMatrix
        
        ## Return this object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        ## Try to get the inverse from the cache
        theInverse = x$getinverse()
        if(!is.null(theInverse)){
                message("getting inverse matrix")
                return(theInverse)
        }
        
        ## In case there is no cache, get the actual matrix, calculate
        ## the inverse, put it in the cache and return it
        matrix <- x$get()
        theInverse <- solve(matrix)
        x$setinverse(theInverse)
        theInverse
}
