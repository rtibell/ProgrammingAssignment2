## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
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
}


m1 <- matrix(c(1,2,3,4),2,2)
mc1 <- makeCacheMatrix(m1)
a1 <- cacheSolve(mc1)
a1 == solve(m1)
a1 <- cacheSolve(mc1)

c2 <- c(1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 0, 1, 4, 2, 3 )
m2 <- matrix(c2, 4,4)
mc2 <- makeCacheMatrix(m2)
a2 <- cacheSolve(mc2)
a2 == solve(m2)
a2 <- cacheSolve(mc2)

