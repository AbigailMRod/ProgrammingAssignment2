#function that caches the inverse of a matrix

#The first function, makeCacheMatrix, creates a special "array", which is actually a list containing a function to
# 1) set array value
# 2) get value of array
# 3) set the value of the inverse of the matrix
# 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##The following function calculates the inverse of the special "matrix" created
# with the previous function. However, first check if the inverse has already
#been calculated. If so, it gets the inverse from the cache and skips the 
#calculation. Otherwise, it computes the inverse of the data and sets the 
#value of the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
