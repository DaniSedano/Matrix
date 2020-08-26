
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




