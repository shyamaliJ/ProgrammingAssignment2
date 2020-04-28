

## This function takes input and creates variable for inverting matrix. For new inputs, 
## this function also resets inv variable to NULL to prevent wrong ca

makeCacheMatrix <- function(x = matrix()) { 
  ## The function takes input x as matrix and inv variable is set to NULL
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x ##function returns value of input, x
setinverse <- function(solve) {inv <<- solve} ##function sets and returns value of inv
getinverse <- function() {inv} ## function gets and returns value of inv 
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
## output of this function is a list so that it can be calles using $ symbol later
}


## the function tries to get inverse of matrix from list. If it gets it, it prints it.If not,
## it calculates the inverse by getting input matrix from 'get' and setting the calculated inverse
## to 'setinverse'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ## gets the inverse value from getinverse of prev. function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ## If inv is not equal to NULL, then return the value with a message
  }
  data <- x$get() ##when inv=NULL get input value x
  inv <- solve(data, ...) ## solve to get inverse of x
  x$setinverse(inv) ## set value of inverse and return inverted matrix
  inv
} 

