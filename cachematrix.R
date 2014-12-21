## The two functions below are used concurrently to find the inverse of a matrix x. 
## If the inverse has already been found, it is returned without the need to recal-
## calculate the inverse, thus, saves computing time.
## NOTE: makeCacheMatrix(x) has to be called prior to cacheSolve(x).

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix x created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  i <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  i <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(i)
  i
}