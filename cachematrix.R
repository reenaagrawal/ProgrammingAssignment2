#makeCacheMatrix function creates a functions that allows users 
# to set/get data as well as set/get matrix inverse of the 
#data

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NUL
  }
  
  get <- function() x
  
  setinverse <- function(inverse)  inv <<- inverse
  
  getinverse  <- function () inv
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## cacheSolve checks first to see if cached inverse of matris is available
# if it available, it prints a message and uses cached version
# if it is not available, it calculates the inverse of the matrix
# and stores it in the object so that it is cached for future reuse.

cacheSolve <- function(x, ...) {
      
  ## Return a matrix that is the inverse of 'x'
  inv  <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get
  inv  <- solve(data, ...)
  x$setinverse(inv)
  inv
}
