## By this functions you can create a cache of matrix and its inverse, what can 
## help you save a lot of time then you need to comput the huge inverted matrix 
## many times 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL 
  m <- NULL 
  #set the original matrix
  set <- function(y) {
    if (class(y)!="matrix") {
      message("You have to set matrix")
    } else if (ncol(y)!=nrow(y)){
      message("You have to set square matrix")
    } else{
      x <<- y  
    }
         
  }
  #print the original matrix
  get <- function() x
  #set an inverted matrix and the original matrix which has been solved
  setsolve <- function(solve, mat) {
    s <<- solve
    m <<- mat
  }
  #print the inverted matrix
  getsolve <- function() s
  #print the original matrix wihich has been solved
  getmat <- function() m
  #list of stored functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve,
       getmat = getmat)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve  retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Getting stored values
  s <- x$getsolve()
  m <- x$getmat()
  #Checking stored data
  if(!is.null(s) & identical(x$get(), m)) {
    message("Getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s, data)
  s
}
