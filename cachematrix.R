## This functions return the inverse of a matrix

## This function keeps the cache of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)

}


## This function returns the inverse of a the spetial matrix from the above function, but if there is an inverse
## in cache, this function returns a message and then the cache value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
