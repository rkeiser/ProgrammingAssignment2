## cacheMatrix contains a pair of functions that cache the inverse
## of a matrix, makeCacheMatrix and cacheSolve.

## The following makeCacheMatrix function that creates a special
## "matrix" object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The following cacheSolve function returns a matrix that is the 
## inverse of the matrix from the makeCacheMatrix function.
##     If the inverse matrix has already been calculated the inverse
##         is retreived from the cache.
##     If not, the inverse is calculated using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
  
}
