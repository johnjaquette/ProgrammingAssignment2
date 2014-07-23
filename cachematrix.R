## makeCacheMatrix takes a matrix as an argument, stored as x. inv represents the inverse of x, 
## initially assigned as NULL. x can be changed using the set function. The get function returns x. 
## inv can be changed using the setinv function. The getinv function returns inv. 

## makeCacheMatrix returns a list containing the set, get, setinv, and getinv functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve takes a makeCacheMatrix list as an argument and returns the inverse of the matrix.
## If inv has already been set, cacheSolve returns inv. Otherwise, cacheSolve computes the 
## inverse using solve(), stores it using setinv, and returns it.

cacheSolve <- function(x, ...) {
  result <- x$getinv()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$get()
  result <- solve(data)
  x$setinv(result)
  result
}
