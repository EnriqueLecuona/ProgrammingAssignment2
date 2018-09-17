## This file contains two function, in conjuction they are use to save time when you have already the inverse of 
## a matrix (if the inverse exists of cuorse). If you have the inverse already save in the result list of the first
## function, then R only searchs in the memory cache for the result and then prints it, if the inverse of the matrix 
## has not been calculated already, then R calculates the inverse and prints it :D .

## Function that creates a list that can save the inverse of a matrix and the matrix itself.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      setmatrix <- function(y){
            x <<- y
            m <<- NULL
      }
      getmatrix <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(setmatrix = setmatrix, getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function prints the saved inverse of the matrix contained in the list introduced, or it computes the inverse if the list
## doesn´t contain the inverse of the matrix, this function is used to save time if the inverse of the matrix has been calculated 
## before.

cacheSolve <- function(x, ...) {
              m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data (inverse of the matrix)")
            return(m)
      }
      data <- x$getmatrix()
      m <- solve(data, ...)
      x$setinverse(m)
      m     ## Return a matrix that is the inverse of 'x'
}

## Thanks for taking your time reading this R file. Have a nice day :D. 
