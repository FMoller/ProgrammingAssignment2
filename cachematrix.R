## First function create a cache for a matrix. 'm' is an objects for internal operations
## and x and y objects to recieve and store the matrix.
## get function returns the matrix stored, set function, recieve a matrix 'y' and store it in an internal
## object x.
## setsolve stores the inverse of stored matrix, getsolve returns it.

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

## cachesolve recieves an stored matrix/inveser. It reads the inverse
## of this matrix, if its already stored, the function only return 
## this stored inverse. Else, the function calculates and store
## inverse function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  else{
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setsolve(m)
    return(m)
  }
}

##Sorry for my very bad english.