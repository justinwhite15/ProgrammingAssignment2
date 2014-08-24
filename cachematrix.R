## Matrix Caching
## These functions 
## @date 8/23/14
## @author github.com/justinwhite15

## makeCacheMatrix
## Function to create cache matrix object
## @parameter   x   matrix to inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL        # Create variable to store cached inverse matrix
  
  # Create all of the functions of the object
  # set - set the base matrix and null out any cached inverse matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Return the base matrix
  get <- function() x
  
  # Store the inversed matrix
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  
  # Get the inversed matrix
  getInverseMatrix <- function() m
  
  #Return the functions as a list
  list(
        set = set, 
        get = get,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix
  )
}


## cacheSolve
## Function to return the inverse of the matrix x and cache the results
## @parameter   x   makeCacheMatrix object
cacheSolve <- function(x) {
  # Get the inversed matrix from the cache matrix
  m <- x$getInverseMatrix()
  
  # Cache-Hit - the cache matrix returns a non-null inversed matrix, so return it
  if(!is.null(m)) {
    return(m)
  }
  
  # Cache-Miss - the cache matrix dosen't return an inversed matrix, so get the original matrix, calculate inverse, cache it, and return it
  data <- x$get()
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}
