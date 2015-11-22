## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
## stores the cached value
inv <- NULL
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
}
## get the value of the matrix
get <- function() x
        ## invert the matrix and store in inv
        setInverse <- function(inverse) inv <<- inverse
        ## get the inverted matrix from inv
        getInverse <- function() inv
        ## return the created functions to the working environment
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## attempt to get the inverse of the matrix stored in inv
inv <- x$getInverse()
        ## return inverted matrix from inv if it exists.If not, create the matrix in working environment
        if (!is.null(inv)) {
                message("getting cached data")
                ## display matrix in console
                return(inv)
        }
        ## create matrix since it does not exist
        mat <- x$get()
        ## set and return inverse of matrix
        inv <- solve(mat, ...)
        ## set inverted matrix in inv
        x$setInverse(inv)
        # display matrix in console
        return(inv)
}

## Below is the result by testing my functions 
## > setwd("~/Desktop/coursera")                       change working directory
## > source("cachematrix.R")                           load R program
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))   create matrix in working environment
## > my_matrix$get()                                   show the created matrix
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(my_matrix)                              1st run returns inverted matrix
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(my_matrix)                              2nd and subsequent runs
## getting cached data                                  returns inverted matrix 
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
