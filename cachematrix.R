# First I am making the makeCache Matrix function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Define the setter for x
  set <- function(y) {
    x <<- y
    m <<- NULL
    # This sets the input, y, to x in the parent environment
    # And assigns m as NULL in the parent environment, clearing 
    # any prior value
  }
  #Define the getter for x
  get <- function() x
  # Setter and getter for m
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  # Returns each of the functions to the parent environment
  # and names them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# Populates and retreives inverses of matricies
cacheSolve <- function(x, ...) {
  # Attempts to get the inverse if already existing
  m <- x$getinverse()
  # Returns it if it is found
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If not found, calculates the inverse and returns
  # the value to the parent environment
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

