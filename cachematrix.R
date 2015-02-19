## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #new value of x, so inverse is as yet unknown
        inv <- NULL
        #reinitialise variables
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #use to return value of x
        get <- function() x
        #store value of inverse
        setinv <- function(inverse) inv <<- inverse
        #return value of inverse
        getinv <- function() inv
        #and return the final list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        #check to see if there is already a value stored for the inverse
        if(!is.null(inv)) {
                #if so, return cached value
                message("getting cached data")
                return(inv)
        }
        #find value of matrix
        data <- x$get()
        #invert the matrix
        inv <- solve(data, ...)
        #store inverse of matrix for future use
        x$setinv(inv)
        #return the inverse
        inv
        
}
