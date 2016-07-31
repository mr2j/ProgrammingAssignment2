# These functions calculate and cache the value of a matrix inversion,
# since its a costly computation, and it doesnt need to be re-calculated
# if the value has stayed the same.


# makeCacheMatrix is a list of functions, later to be used
# to set and get the value of the matrix x. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


# cacheSolve returns a cached version of m, if it has been calculated previously.
# It returns the inverse of the set matrix x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

# Runs like this:
# a <- makeCacheMatrix()
# a$set(matrix(1:4,2,2))
# cacheSolve(a)

# Johnny Jensen