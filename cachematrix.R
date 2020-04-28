
## The function takes input x as matrix and the matrix is assigned as y in the environment inside the 
## set function and m is reset to NULL.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function(){x}
setmean <- function(solve) {m <<- solve}
getmean <- function() {m}
list(set = set, get = get,
     setmean = setmean,
     getmean = getmean)

}



## the function tries to get inverse of matrix from list. If it gets it, it prints it.If not,
## it calculates the inverse by getting input matrix from 'get' and setting the calculated inverse
## to 'setmean'

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
} ## Return a matrix that is the inverse of 'x'

