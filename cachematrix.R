## Functions allow for the storage of a matrix and its inverse value in a cache,
## which can be retrieved and changed through get and set functions.



## Stores matrix in cache with get/set functions for original matrix and inverse matrix

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL # variable to store inverse matrix
  set <- function(mat_update) { # function to initiate/reassign variables for cache (set new matrix with no solution for inverse)
    mat <<- mat_update
    inv <<- NULL
  }
  get <- function() mat # returns original matrix
  setinv <- function(inv) inv <<- inv # updates inverse of matrix with provided solution
  getinv <- function() inv # returns inverse of matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) # returns labeled list of contents
}



## Either retrieves the inverse matrix from cache or solves for the inverse matrix and updates the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # attempts to get inverse of matrix from cache
  if(!is.null(inv)) { # checks cache for previous result, returns if present
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # obtain original matrix
  inv <- solve(data, ...) # solve for inverse, store in local variable
  x$setinv(inv) # update cache
  inv # return local variable with solution
}