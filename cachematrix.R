## This function creates a special "matrix", which is really a list containing a function to: 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get<-function() x
        setinverse<-function(solve) inv <<- solve
        getinverse<-function() inv
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## This function checks if the inverse of a matrix is already calculated
## When it is, it returns the inverse matrix from cache, otherwise it will be calculated.
## Assumption is that the matrix is invertible

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix.")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}
        ## Return a matrix that is the inverse of 'x'
}