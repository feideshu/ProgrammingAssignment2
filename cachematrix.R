## Two functions are created in this file
## makeCacheMatrix takes a matrix and returns a list containing $set $get $setmean and $getmean
## it uses << to assign values in the global environment rather than within the function, which then is useful to retrieve 
## directly from the cache

## cacheSolve either computes the inverse ("solve") of a matrix, or simply reads it from memory if the solve 
## has been done already.

## called one after the other as follows, assuming there exists a square invertible matrix in the environment: 
## mname <- makeCacheMatix(squaremat)
## cacheSolve(mname)

## Write a short comment describing this function
# This is exactly the function makeVector - just names of variables were changed
# to reflect that this computes a "solve" rather than a "mean" and thus uses "s" rather than "m" for clarity
# more comments inside
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## This function should be applied to the output of makeCacheMatrix
## It runs a "solve" on the matrix if the solve has not been computed before
## but if it already has been computed, it just retreives it from "s"

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
