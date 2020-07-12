## Put comments here that give an overall description of what your
## functions do

## This function create a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  #Storing the functions in list make it easier to work
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function returns the inverse of the special matix.
## If the inverse is null, then it will calculate its inverse and returns its value
## Otherwise, the cached matrix will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
