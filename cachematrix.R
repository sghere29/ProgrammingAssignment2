# Creates a matrix that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL # stores inverse 
set <- function(y) {
  x <<- y # update matrix
  inv <<- NULL # reset inverse cache
}
get <- function() x  # get matrix
setinverse <- function(inverse) inv <<- inverse  # save
getinverse <- function() inv   # get inverse
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)  # saved as a list
}



# computes inverse of a matrix or gets cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # check for cached inverse
  
  if (!is.null(inv)) {
    message("getting cached data")   # return cached inverse with a msg
    return(inv)
  }
  mat <- x$get()  # get matrix
  inv <- solve(mat, ...)  # calculate inverse of the input matrix
  x$setinverse(inv)   # cache the output
  inv            # return the output
  
  }
