## These two functions show how to cache values.

## makeCacheMatrix outputs a list containing functions to get and set matrix and 
## inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve inveses the matrix - it inverses if it is not done before.
## if the matrix was already inversed, then reads from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## Testing - m <- matrix (c(3,4,0,0,2,3,0,0,0,0,6,7,0,0,5,6), nrow=4, ncol=4)
## mt <- makeCacheMatrix(m)
## cacheSolve(mt) ## computes fresh
## cacheSolve(mt) ## gets it from the cache


