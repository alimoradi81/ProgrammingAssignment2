# 'makeCacheMatrix' contains 4 function to set value of a matrix and its inverse in the cache as well as to get their
# valus.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(invvalue) matinv <<- invvalue
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The 'cachinv' function first check whether the inverse of the matrix is available in cache or not.
# If yes, reload it. If not, calculate it and save it in the cashe. Saving in cahce is done using functions
# defined in 'makeCacheMatrix' function.

cachinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
