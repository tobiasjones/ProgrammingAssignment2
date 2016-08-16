## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly (there 
## are also alternatives to matrix inversion that we will not discuss here). Your 
## assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
# The first function, makeCacheMatrix creates a special "matrix", which is really 
# a list containing a function to
# 
# set - set the value of the matrix
# get - get the value of the matrix
# setinverse - set the value of the inverse of the matrix
# getinverse - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(
        set = set, 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse
    )
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}   

## Sample Runs
# myMatrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
# myMatrix
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
# myCache <- makeCacheMatrix(myMatrix)
# cacheSolve(myCache)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
 
 
# myMatrix2 <- diag(4,10)
# myMatrix2
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,]    4    0    0    0    0    0    0    0    0     0
##  [2,]    0    4    0    0    0    0    0    0    0     0
##  [3,]    0    0    4    0    0    0    0    0    0     0
##  [4,]    0    0    0    4    0    0    0    0    0     0
##  [5,]    0    0    0    0    4    0    0    0    0     0
##  [6,]    0    0    0    0    0    4    0    0    0     0
##  [7,]    0    0    0    0    0    0    4    0    0     0
##  [8,]    0    0    0    0    0    0    0    4    0     0
##  [9,]    0    0    0    0    0    0    0    0    4     0
## [10,]    0    0    0    0    0    0    0    0    0     4
# myCache2 <- makeCacheMatrix(myMatrix2)
# cacheSolve(myCache2)
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,] 0.25 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
##  [2,] 0.00 0.25 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00
##  [3,] 0.00 0.00 0.25 0.00 0.00 0.00 0.00 0.00 0.00  0.00
##  [4,] 0.00 0.00 0.00 0.25 0.00 0.00 0.00 0.00 0.00  0.00
##  [5,] 0.00 0.00 0.00 0.00 0.25 0.00 0.00 0.00 0.00  0.00
##  [6,] 0.00 0.00 0.00 0.00 0.00 0.25 0.00 0.00 0.00  0.00
##  [7,] 0.00 0.00 0.00 0.00 0.00 0.00 0.25 0.00 0.00  0.00
##  [8,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.25 0.00  0.00
##  [9,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.25  0.00
## [10,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.25