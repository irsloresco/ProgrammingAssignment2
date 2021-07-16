makeCacheMatrix <- function(x = matrix()) { ## define the argument with default of matrix
  inv <- NULL                             ## start inv as NULL
  set <- function(y) {                    ## define the set func to assign new 
    x <<- y                             ## meaning of the variable of the matrix in parent environment
    inv <<- NULL                        ## resets inv to null
  }
  get <- function() x                     ## defines the get func which returns value of the matrix
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## refer to the func with $ op 
}




cacheSolve <- function(x, ...) {
  ## to return a matrix, inverse the 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
