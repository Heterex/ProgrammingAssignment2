## calculate the inverse of a matrix, cache the solution, and recall it if needed again 
## without having to recalucate it every time it is asked for in code

## creates a matrix to both set and get the inverse of a matrix when needed 
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## sets the value of vector
  
  get <- function() x
  ## gets the value of the vector
  
  setinv <- function(solve) m <<- solve
  ## sets the value of the inverse of the matrix
  
  getinv <- function() m
  ## gets the value of the inverse of the matrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
} 

## creates the inverse of a matrix or obtains it from the cached data 
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getinv()
  ## checks to see if the inverse has already been cached
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the inverse has already been cached (is NOT NULL) a message "getting cached data" is displayed
  ## while that information is being extracted
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
} 
