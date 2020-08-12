#The makeCacheMatrix function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #inverse is null initially
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse} #set values of the inverse
        getInverse <- function() {inv} #get values of the inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#The cacheSolve function computes the inverse of the matrix returned in makeCacheMatrix,
#and returns the cached inverse if it's already been calculated

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){ #check to see if inverse is empty
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv #return matrix that is inverse of x
}



