## Caching the Inverse of a Matrix:
## Here are two functions that are used to caches its inverse.

## This function can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setInverseMatrix <- function(inverse) invMatrix <<- inverse
  getInverseMatrix <- function() invMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse matrix created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverseMatrix()
  
  if (!is.null(invMatrix)) {
    message("Cache Data Get!")
    return(invMatrix)
  }
  
  mat <- x$getMatrix()
  invMatrix <- solve(mat, ...)
  x$setInverseMatrix(invMatrix)
  invMatrix
}
