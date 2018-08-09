## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() invmat
  list (
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## This function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed, then the
## cacheSolve function will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if (!is.null(invmat)) {
    message("getting cached inverse matrix")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}
