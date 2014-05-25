
## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
#1   set the Matrix
#2   get the Matrix
#3	 set the Inverse of a matrix
#4   get the Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y,...) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the Inverse of the special "Matrix" created with the above function. 
##However, it first checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the computation. 
##Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
         m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
