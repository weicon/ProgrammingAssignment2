## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by the first function, 
## if the inverse has not already been calculated and cached previously. If the inverse has been cached,
## then this function would retrieve and return the inverse from the cache.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  set <- function(y){
    x <<- y
    InverseMatrix  <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(m){
    InverseMatrix <<- m
  }
  getInverseMatrix <- function() InverseMatrix
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve would retrieve and return the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getInverseMatrix()
  if (!is.null(InverseMatrix)) {
    message("getting cached data")
    return(InverseMatrix)
  }
  data <- x$get()
  InverseMatrix <- solve(data, ...)
  x$setInverseMatrix(InverseMatrix)
  InverseMatrix
}
