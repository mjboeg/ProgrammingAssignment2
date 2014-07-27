## These are functions used to initialize and operate on CacheMatrix objects.
## A CacheMatrix object is a list containing the functions set.mat(), set.inv(),
## get.mat(), and get.inv(). These functions all operate in the same environment
## that is created with the makeCacheMatrix function. The defining environment 
## stores the value for the matrix and its inverse used by all the function objects
## in the list.

## makeCacheMatrix creates the environment that stores the current value of the
## matrix and its inverse. Its output is a list containing functions to set and get
## the value of the matrix and the inverse from its location in the makeCacheMatrix
## environment.

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


## cacheSolve takes and cacheMatrix object and returns the cached inverse. If a 
## cached inverse does not exist, it is calculated, stored in the cacheMatrix
## object, and returned.

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
