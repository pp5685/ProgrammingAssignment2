## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #creating matrix cache
  m<- NULL
  #setting the value of matrix
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  #getting the value of matrix
  get<- function() x
  #setting the value of the inverse
  setinv<- function(inv) m<<-inv
  #getting the value of inverse
  getinv<- function() m
  #list of important variables
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
