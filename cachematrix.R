## makeCacheMatrix and cacheSolve are a pair of functions which work together to store (cache) a global value of 
## an expensive computaton, such as the inverse of a matrix.  MakeCacheMatrix stores the initial matrix, and a
## placeholder for the matrix inverse. for When the expensive computation is performed 
## by cacheSolve the cache is checked first, to see if the computation has already been performed, and if it has,
# the result is retrieved directly.  If the computation has not been performed, it is done and hten stored in the cache.

## Create an object which stores the initial matrix, x, and a place holder for m, which is the 
##  the matrix inverse. The place holder is initialised to NULL. 
makeCacheMatrix <- function(x = matrix()) {
  #store as data frames
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }  
  get <- function() x # retrieves x from cache
  setinv <- function(inv) i <<- inv # setinv is a fuction which puts inverse of x into i
  getinv <- function() i # getinv is a function that retrieves inverse of matrix from cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of a matrix, either from a cache if has already been calculated, or by calculation if not.
## If it has been calculated, the inverse is stored in cache. 

cacheSolve <- function(x= matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
 
  i <-x$getinv() # get value of inverse from cache, (NULL, if not already calculated)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}