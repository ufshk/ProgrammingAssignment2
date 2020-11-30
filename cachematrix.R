## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix: takes in a matrix, builds a list of the matrix
#                  and its inverse for easy access
# cacheSolve: returns the inverse of the matrix; if already solved
#             returns cached value, otherwise solves for inverse

## Write a short comment describing this function
# makeCacheMatrix:
#     input: x - a matrix, assumed to be invertible
#     processing: builds a list of attributes for a matrix
#     output: list of attributes of matrix
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve:
#     inputs: x - a list of attributes of a matrix
#             ... - extra parameters passed to the solve function
#     processing: checks if x has a cached value for the inverse of
#                 the matrix, otherwise solves for the inverse
#     output: inverse of the matrix
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$inverse
      if(!is.null(inv)) {
            return(inv)
      }
      data <- x$get
      inv <- solve(x, ...)
      x$setinverse(inv)
      inv
}
