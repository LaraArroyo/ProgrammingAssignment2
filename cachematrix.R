## These functions were coded in order to complete R Programming Assignment 2
## It includes two functions (named makeCacheMatrix and cacheSolve)
## They cache the inverse of a matrix (assuming the supllied matrix is invertible)
## Based on the explanations of the assignment (introduction), among others. 
## Further explanations below


## makeCacheMatrix creates a list of four functions (set, get, setSolve and getSolve) that can be used to 
## get (or set) a cache of the matrix, and its inverse 

makeCacheMatrix <- function(x = matrix()) {
      Sol <- NULL
      set <- function (y) {
            x <<- y
            Sol <<- NULL
      }
      get <- function () x
      setSolve <- function (solve) Sol <<- solve
      getSolve <- function () Sol
      list (set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


## cachesolve checks if the inverse of the matrix has been stored. If so, it does not calculate it again
## if the inverse of that matrix does not exist, then the function estimates and caches the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      Sol <- x$getSolve ()
      if (!is.null(Sol)) {
            message ("getting cached data")
            return (Sol)
      }
      data <- x$get()
      Sol <- solve (data, ...)
      x$setSolve (Sol)
      Sol
}
