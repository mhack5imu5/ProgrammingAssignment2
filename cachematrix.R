## This functions create a special matrix and attach set/get methods to it. 

## This function creates the matrix we want to calculate the inverse of. The content of the matrix as well as its inverse
# can be accessed through the set/get methods

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function checks whether an inverse for the matrix has already been computed: if not it calculates it and stores it
# otherwise is just retrieves it ("if" arrgument is true)
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
