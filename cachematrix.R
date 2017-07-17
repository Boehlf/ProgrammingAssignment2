## The functions calculate the inverse of a matrix  Put comments here that give an overall description of what your
## functions do

## The function makeCachematrix takes a variable of class matrix and 
# calculates its inverse. It returns four fuctions, sc. set, get, setsolve and getsolve,
# as well as the variable x and m.

makeCacheMatrix <- function(x = matrix()) {
  
  # setting the variable
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #defining the functions
  
  get <- function() {x}
  setsolve <- function(solve) {m <<- solve}
  getsolve <- function() {m}
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cachSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #check whether there's cached data and return the variable
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if not chached, then inverse the matrix and return the variable
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
