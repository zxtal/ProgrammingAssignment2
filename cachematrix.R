## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function 'makeCacheMatrix' contains a bunch of functions that prep the data.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL        # i at first is set to null
  set <- function(y) {
    x <<- y
    i <<- NULL  
  }
  get <- function() x    
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# The second function 'cacheSolve' firstly check if 'inverse' is cached. If yes, it will retrieve from result from cache. If not, it will calculate the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {     # check if i is null.
    message("getting cached data")
    return(i)
  }
  data <- x$get() 
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
#test