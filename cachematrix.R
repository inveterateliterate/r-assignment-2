##These functions together work to reduce processing time for calculating the inverse of a matrix
##by pulling a cached version of an already computed matrix inverse
##if the matrix of that inverse has not yet been calculated, it will calculate it, and 
##store it in the cache


##IR: The first function that actually finds the inverse of a given matrix
##and stores it in a new matrix

makeCacheMatrix <- function(x = matrix()) {
  ##creating and getting matrices
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##setting and getting the inverse of the given matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
      setinverse = setinverse, 
      getinverse = getinverse)
}



##IR: The second function searches the cache to determine whether the inverse of this matrix
##has already been calculated. If so, it returns it from the cache, if not
##it finds the inverse
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ##determine if the inverse has been cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if the inverse wasn't cached, find the inverse and return it
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
