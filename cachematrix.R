## These functions store a matrix and calculate the inverse
## if not already done so
## when inverse is calculated it is returned
## matrix needs to be invertable!

## Use: my_environment <- makeCacheMatrix(my_matrix)
##      cacheSolve(my_environment)

## This function stores and retrieves a matrix

## Input: matrix x

makeCacheMatrix <- function(x = matrix())
{
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(self_inverse) inv_matrix <<- self_inverse
    getinverse <- function() inv_matrix
    ## list provides access to getter and setter functions
    list(set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}


## Returns a matrix that is the inverse of a matrix
## handed over in function makeCacheMatrix(x)

## First, the inverse is fetched
## if the inverse exists a message is returned and calculation skipped
## otherwise the matrix is fetched from makeCacheMatrix
## and the inverse is calcuated and returned

## Use: cacheSolve(x)
##      where x is the function makeCacheMatrix(my_matrix)

cacheSolve <- function(x, ...)
{
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix))
    {
        message("getting cached inverse")
        return(inv_matrix)
    }
    coeff_matrix <- x$get()
    inv_matrix <- solve(coeff_matrix, ...)
    x$setinverse(inv_matrix)
    inv_matrix
}