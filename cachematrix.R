## The functions in this package compute and cache the inverse of a matrix

## The function makeCacheMatrix creates a variable i that caches the result of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve obtains the inverse and puts it into its parent environment in i
## when a$getinverse() is called on a matrix created by makeCacheMatrix, it checks if any value is already in i and retrieves it, otherwise computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
