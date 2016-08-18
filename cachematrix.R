makeCacheMatrix <- function(x = matrix()) {
  matt <- NULL
  set <- function(y) {
  x <<- y
  matt <<- NULL
  }
  get <- function() x
  set_inverse <- function(cacheSolve) matt <<- cacheSolve
  get_inverse <- function() matt
  list(set = set, get = get,set_inverse = set_inverse,get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    #message("DEBUG-TEST")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$set_inverse(m)
  m
}
