## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix has four functions named set,get,setinverse,getinverse
## set() - set the value of the matrix
## get() -get the value of the matrix
## setinverse() - set the value of inverse of the matrix
## getinverse() - get the value of inverse of the matrix
## makeCacheMatrix returns the list object containing these four functions

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
    {
      x <<- y
      inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverseValue) inv <<- inverseValue
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## cacheSolve function returns the inverse of the function.
## It checks whether the cache was created previously
## If it finds previously created cache value,it returns that previous value
## If no previous cache value is found, it calculates inverse using solve() and returns the resultant inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) 
    {
      message("getting cached data.")
      return(inv)
    }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
