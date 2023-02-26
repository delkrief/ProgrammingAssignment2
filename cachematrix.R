## write a pair of functions that create a matrix andd cache the inverted matrix
##his function creates a special "matrix" object that can cache its inverse. can compute inverse with solve(x)
#  use the <<- operator to assign a value to an object that is from a different environment
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache.
  
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
    
  }
  #otherwide, calculate the inverse, cache it and return it
  data <- x$get()
  inv <-solve(data, ...)
  x$setinv(inv)
  inv
}
