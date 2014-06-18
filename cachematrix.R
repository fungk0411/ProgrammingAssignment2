## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an object that is matrix and be able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## This function computes the inverse of a matrix returned by makeCacheMatrix.
## If the inverse is already calculated, then the function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if(!is.null(m)){
      print("getting cached data")
      return(m)
  }
  
  data <-x$get()
  m<- solve(matrix, ...)
  x$setInverse(m)
  
  return(m)
}
