## Put comments here that give an overall description of what your
## functions do
##
## Programming Assignment Two - R Programming
## Jonathan Pengelly
## 
## This script contains two functions. The first function
## 'makeCacheMatrix' builds a list which contains four
## functions used for setting and getting the matrix
## and inverse matrix values within the parent scope (the
## environment calling the function). The second function
## cacheSolve returns the inverse value for the special matrix
## object. If previously calculated, then this value will be 
## returned from the cache, otherwise it will be calculated,
## saved in cache and then returnedb
##

## Write a short comment describing this function
##
## This function builds a special matrix object (a
## list) which contains four functions. Firstly the
## local inverse matrix is reset. Then the set
## function is defined which sets the x variable in
## the parent scope. Next the get function returns
## the x value that is stored within the parent scope.
## The setinv function sets the inverse of the matrix
## to the inv variable within the parent scope. The
## getinv function returns the value of the inv
## variable within the parent scope. Lastly the special
## matrix object is returned.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  } 
  
  setinv <- function(inverse){
    inv <<- inverse
  }
  
  getinv <- function(){
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
##
## This function first checks to see if an inverse
## matrix has been computed and stored in cache.
## If so then the cached value is returned. 
## If not then the inverse matrix is computed, the
## inverse matrix is stored in cache and lastly 
## the inverse matrix value is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  
  if (!is.null(inv)) {
    ## message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}
