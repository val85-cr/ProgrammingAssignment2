## makeCacheMatrix will create a matrix ('x') and will retrieve its inverse by means of 
## the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  ## Create variable, m, in local environment (makeCacheMatrix)
  m <- NULL
  
  ## When set(y) is called, y value is superassigned to x, where x is the input value of 
  ## makeCacheMatrix. That means that the initial matrix 'x', will be replaced for a new one ('y'),
  ## so the initial variable m must be recalculated for this new matrix. Therefore m is 
  ## reinitialized to NULL.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get() returns the matrix 'x'
  get <- function() x
  
  ## setinverse(inverse), changes the value of m in the local environment (makeCacheMatrix) 
  ## to the given value (inverse)
  setinverse <- function(inverse) m <<- inverse
  
  ## getinverse() returns the inverse of the matrix 'x'
  getinverse <- function() m
  
  ## list allows these functions to be called outside the local environment
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will compute, cache and return the matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## First we call the getinverse() function from x, where x is a list of functions defined in
  ## the makeCacheMatrix environment. 
  m <- x$getinverse()
  
  ## Then, we check if the variable m has already a matrix stored (i.e., cached). If this solves
  ## to TRUE, then it returns that matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## The following piece of code will be executed only when the previous if statement does not
  ## solve to TRUE (meaning that there is no inverse matrix stored, so it has to be
  ## calculated)
  
  ## by calling get() function from x, the matrix x from the makeCacheMatrix environment
  ## is retrieved and assigned to a new local variable called 'data'
  data <- x$get()
  
  ## inverse matrix of 'x' is computed...
  m <- solve(data, ...)
  
  ## ... and m is updated in makeCacheMatrix by calling the function setinverse(inverse) of x
  ## (setinverse(inverse) superassigns the new computed matrix to m in makeCacheMatrix)
  x$setinverse(m)
  
  ## Print the new inverse matrix
  m
        
}

