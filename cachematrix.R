## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 

## The function, makeCacheMatrix creates a special "matrix", containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value of the inverse in the 
## cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
