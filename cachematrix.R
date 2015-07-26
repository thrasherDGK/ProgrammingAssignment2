## Put comments here that give an overall description of what your
## functions do

## Inside closure defines a value for inverse_matrix (by default it is an empty one)
## Provides methods for getting/setting values for matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- matrix()
  set <- function(new_value) {
    x <<- new_value
    inverse_matrix <<- matrix()
  }
  get <- function() x
  setInverse <- function(solve) inverse_matrix <<- solve
  getInverse <- function() inverse_matrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function checks if inverse_matrix for x is identical with new matrix
## If so, then it invokes solve on passed data and sets inverse_matrix to returned value

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getInverse()
  if(!identical(inverse_matrix, matrix())) {
    message('getting cached data')
    return(inverse_matrix)
  }
  
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
