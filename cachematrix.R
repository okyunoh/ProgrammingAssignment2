## Put comments here that give an overall description of what your
## functions do
## I just modified the example codes, which may store and retrieve the mean in cache.

## Write a short comment describing this function
## First function, I make a two by two matrix consisted with functions
## 1. set the value of matrix, 
## 2. get the value of matrix, 
## 3. set the value of the inversion of matrix, 
## 4. set the value of the inversion of matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        matrix(c(set, get, setinv, getinv), 2,2)
}


## Write a short comment describing this function
## Before calculating the inversion of matrix, this function first checks to see
## whether already calculated inversion exists or not. 
## If it exists, it get the inversion matrix from cache and skip the calculation.
## Otherwise, it calculates the inversion matrix using solv() function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x[4][[1]]()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x[2][[1]]()
        inv <- solve(data, ...)
        x[3][[1]](inv)
        inv

}
