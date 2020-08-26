##This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) I <<- inverse
  getsolve <- function() I
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##This function computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(I)
  I
}




