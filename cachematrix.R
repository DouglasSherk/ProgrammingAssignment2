# cachematrix
# A set of functions for computing and caching the inverse
# of a matrix. Useful mostly for very large matrices.

##
# makeCacheMatrix
#  Creates a container for a cacheable matrix and provides
#  helper methods for getting/setting its computed solution
#  (inverse).
#
# @param x {matrix} the matrix to compute the inverse of
#
# @return {list{set, get, setinverse, getinverse}}
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##
# cacheSolve
#  Solves (gets the inverse of) a matrix and saves it to a cache,
#  or gets it from the cache if it has already been computed.
#
# NOTE: Prints "getting cached data" when fetching from the cache.
#  
# @param x {list{set, get, setinverse, getinverse}}
#  Data store returned by `makeCacheMatrix` with methods to
#  check for the existence of a cached result and set it after
#  computation as needed.
#
# @return {matrix} the inverse of the matrix stored in `x`
##
cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}