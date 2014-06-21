## The Two functions provide the inverse of a square matrix. 
## Since, the square of the matrix can take a long time to compute, the square of the matrix is cached when created.
## If the input matrix has already been solved, the cached data is used instead of recomputing the inverse of the matrix

## This function should be run and saved as an object.
## The make CacheMatrix caches the inverse of the matrix for later use.

makeCacheMatrix <- function (x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- Null
    
  }
  get<-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function checks the object saved by the makeCacheMatrix and checks to see if the matrix has been previously cached.
## If the matrix has been cached, the cached data is returned. If the matrix has not yet been solved, the inverse is computed.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
