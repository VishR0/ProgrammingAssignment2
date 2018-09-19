## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Create the matrix and store inverse that will be used in the solver
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = get,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
##Returns the matrix inverse of X, using a cache if one exists
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  inv
}
