## The following two functions use to reduce a computation by caching the inverse of a matrix instead to repeatedly compute it
## The first function, makeCacheMatrix creates a special "matrix" object that can cache the input matrix and its inverse

## Set the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
# initialize to NULL
  cache <- NULL
# create the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
## Get the value of the matrix
  get <- function() x
## Set the value of the inverse matrix
  setinv <- function(inv) cache <<- inv
## Get the value of the inverse matrix
  getinv <- function() cache
## Return functins
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}
## Function cacheSolve() returns inverse matrix of special "matrix" created with the above function
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache
## If there is no calculated inverse matrix in the cache then cacheSolve() function calculates the inverse matrix and sets it in the cache using the setinv function.

cacheSolve <- function(x, ...) {
## If an inverse has already been calculated this gets it
  cache <- x$getinv()
## Return inverted matrix from cache if it exists
## Else create the matrix
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
## Run the get function to get the value of the input matrix
  val_matrix <- x$get()
## Compute the value of the inverse of the input matrix
  cache <- solve(val_matrix, ...)
## Set inverted matrix in cache
  x$setinv(cache)
## Return the inverse
  cache
}
