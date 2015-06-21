makeCacheMatrix <- function(x=matrix()) {
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve)
    m <<- solve
  getinv <- function () m
  list(set= set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(x=matrix()) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

a<-makeCacheMatrix()
a$set(matrix(c(1,2,2,1),2,2))
cacheSolve(a)
