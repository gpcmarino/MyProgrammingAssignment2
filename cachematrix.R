## We will write a pair of functions "makeCacheMatrix" and "cacheSolve" 
## that will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  u <- NULL
  sett <- function(w) {
    x <<- w
    u <<- NULL
  }
  gett <- function() x
  settsolve <- function(solvve) u <<- solvve
  gettsolve <- function() u
  list(sett = sett, gett = gett,
       settsolve = settsolve,
       gettsolve = gettsolve)
}


## This function computes the inverse of the special "matrix". 

cacheSolve <- function(x, ...) {
  u <- x$gettsolve()
  if(!is.null(u)) {
    message("getting inversed matrix")
    ## Return a matrix that is the inverse of 'x'
    return(u)
  }
  datum <- x$gett()
  u <- solve(datum, ...)
  x$settsolve(u)
  u
}