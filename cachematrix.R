## Comments and Descriptions are mentioned as a Sub-Title, as well as Within the code as well.

## makeCacheMatrix will be requiring a matrix as an input, and the matrix's inverse will be saved in the vector.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL         # Initialize Inverse
  
  # SET matrix in Global Enviornment
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x      # Function to get the matrix
  setinverse = function(inverse) inv <<- inverse   # Function uses Inverse to solve value
  getinverse = function() inv            # Gets inverse
  
  # List of functions to return
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Computes inverse of "special" matrix object
# cacheSolve will first check, whether the matrix is already saved in the Cache or not. If yes, then it will take the output from there
# only, otherwise, it will solve the new matrix which isn't saved in the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()                # gets cached matrix back
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If no inverse, get matrix stored in object
  data = x$get()
  inv = solve(data, ...)  # Matrix multiplication for inverse
  
  x$setinverse(inv)       # Set inverse to object
  inv
}
