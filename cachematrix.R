## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Purpose of this function : 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function

## This funcion return the inverse of a matrix calculated previously with makeCacheMatrix.
## In the case the inverse of this matrix has already been calculated, it just look for the value 
## of the inverse in the cache and return it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}
