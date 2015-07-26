## This scipt contains pair of functions that cache the inverse of a matrix.
## 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve will retrieve the inverse from the cache.
##
## NOTE: These functions assume that the given matrix is square and invertible.
##       Computation of the inverse is done with the R function 'solve'. 
##
## History: 
##      2015-07-25  J.Anderson  file created
##

## `makeCacheMatrix` creates an "matrix" object (list) that can
## cache a matrix and the inverse of the matrix.  The object
## contains the following functions:
##   
##   1.  set - set the matrix
##   2.  get - get the matrix
##   3.  setinverse - set the inverse of the matrix
##   4.  getinverse - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL         ## intialize the inverse value
    set <- function (y) {
        x <<- y             ## cache a new matrix value
        inverse <<- NULL    ## reset the inverse value
    }
    get <- function() x     ## return the matrix value
    setinverse <- function(y) {
        inverse <<- y       ## cache the inverse value
    }
    getinverse <- function() inverse    ## return the inverse value
    list(set = set,         ## return the list of functions that define this object
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## 'cacheSolve' calculates the inverse of a matrix cached in the 
## object created with the above function, 'makeCacheMatrix'. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via the setinverse function.
## NOTE: assumes that x is always invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()       ## get inverse from cache object 'x'
    if(!is.null(inverse)) {         ## check if cached inverse exists
            message("getting cached data")
            return(inverse)         ## return cached inverse if it exists
    }
    matrix <- x$get()               ## otherwise, get the matrix
    inverse <- solve(matrix, ...)   ## calculate the inverse (assumes x is invertible)
    x$setinverse(inverse)           ## cache the inverse on the cache object 'x'
    inverse                         ## return the inverse
}
