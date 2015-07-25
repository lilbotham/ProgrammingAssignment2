## Return inverse of a matrix and save inverse value
## for future access without invoking the solve() function

## input:  x is invertible matrix
## output:  returns list of 4 functions related
##          to checking values of global variables (cache)
##          and computing inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## input:  a list of functions as defined in makeCacheMatrix
## output:  returns stored val of matrix inverse if was 
##          previously computed and stored in global var inv 
##          o/w computes matrix inverse, updates global var inv,
##          and returns value of inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)  ## execution of cacheSolve stops
  }
  ## executes if inverse has not been previously computed
  data <- x$get() 
  inv <- solve(data, ...) ##compute inverse of matrix
  x$setinv(inv)  ## update value of global var inv for future use
  inv
}