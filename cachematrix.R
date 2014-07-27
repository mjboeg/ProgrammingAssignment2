## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set.mat <- function(new.mat) {
    x <<- new_mat
    inv <<- NULL
  }
  get.mat <- function() x
  set.inv <- function(new.inv) inv <<- new.inv
  get.inv <- function() inv
  list(set.mat = set.mat, get.mat = get.mat, 
       set.inv = set.inv, get.inv = get.inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        x.inv <- x$get.inv()
        if(!is.null(x.inv)) {
          message("cacheSolve: getting Inverse from cache")
          return(x.inv)
        }
        x.mat <- x$get.mat()
        x.inv <- solve(x.mat, ...)
        x$set.inv(x.inv)
        x.inv
}
