makeCacheMatrix <- function(x = matrix()) {
  ## First define the cache 'inv' set to NULL
  inv <- NULL
  ## Next, assign the input matrix y to the variable x in the parent environment (<<-)
  set <- function(y) {
    x <<- y
    ## Re-initialize the inv to NULL in the parent environment
    inv <<- NULL
  }
  
  get <- function() x ## Get the matrix 'x'
  setInverse <- function(inverse) inv <<- inverse ##Set the cache 'inv' to the inverse of matrix 'x'
  getInverse <- function() inv ## Return the cahced inverse of 'x'
  
  ## Return the list of functions for the matrix
  list(set = set,
       get = get,
       setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of a matrix supplied above
## The function will retrieve the inverse from
## the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
  ## Return the inverse of the matrix 'x'
  inv <- x$getInverse()
  
  ## Return the cached matrix if it's already been computed
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Compute the inverse of the matrix
  data <- x$get() ##Get it
  inv <- solve(data, ...) ##Solve it
  x$setInverse(inv) ##Set it
  inv ##Return it
}
