## The combination of these functions will take in an input matrix, determine if the inverse
## has been solved before and stored in cache, solve it's inverse if it is not in cache, then return 
## the newly solved inverse or the cached if it exists.

## makeCacheMatrix takes an matrix input, reverses it, and store in cache
makeCacheMatrix <- function(x = matrix()) {
  
  # Set Default valyes to NULL
  m <- NULL
  y <- NULL
  
  # Set values of the Matrix
  setMat <-function(y) {
          x <<- y
          m <<- NULL
        }
 
  # get the Matrix
  getMat <- function() x 
  
  # compute matrix inverse
  setInv <- function(solve) m <<- solve
  
  # retrieve inverse 
  getInv <- function() m
  list(setMat=setMat, getMat=getMat,
       setInv=setInv,
       getInv=getInv)
}

## cacheSolve function determines if matrix inverse has been cached
cacheSolve <- function(x=matrix(), ...) {
  
  # pulls calculated inverse if it's been calc'ed already
  m <- x$getInv()
  
  # if inverse is already calc'ed, pull the cached data
  if(!is.null(m)){
            message("getting cached data")
            return(m)
  }
  
  # get input matrix if inverse is not cached already
  y <- x$getMat()
  
  # get inverse of input matrix if inverse is not cached already
  m <- solve(y, ...)
  
  # cached the inversed input matrix if the inverse is not cached already
  x$setInv(m)
  
  # display the results
  m
}