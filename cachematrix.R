#The two functions below create an object, storing a matrix and its inverse,
#and a method to set/get the inverse in this object.

#This function defines getters and setters for the object containing a matrix and its inverse
makeCacheMatrix <- function(M = matrix()) {
  inv <- NULL
  set <- function(y) {
    M <<- y
    inv <<- NULL
  }
  get <- function() M
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function gets the inverse of a matrix stored in a makeCacheMatrix object,
#by setting it if not already calculated or directly returning it otherwise
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- M$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
