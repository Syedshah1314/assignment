## This function creates an inverse of a matrix given as input
## 

## The first function, creates a matrix which is a list to set& get matrix, set & get inverse



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function calculates inverse of matrix created by the above function
## This function also checks to see inverse is already calculated, if so it gets from cache 
## otherwise does the inverse calculation and sets the value of inverse

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i   ## Return a matrix that is the inverse of 'x'
}
