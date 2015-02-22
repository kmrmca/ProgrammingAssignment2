## These set of two functions help in storing a matrix and caching its inverse. The main purpose is to show 
## the advantage of R scoping rules and how it helps in preserving the state in an R object


## This function sets the value, gets the value, sets the inverse 
## and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function calculates the inverse of the matrix with makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  ## Checks if the inverse has already been calculated and if so gets it from the cache
  
  if(!is.null(m)) {
    message("Getting Inverse Matrix")
    return(m)
  }
  
  ## If inverse has not been calculated calculate the inverse and the set the value
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}

