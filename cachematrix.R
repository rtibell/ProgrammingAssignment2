## These functions handle the computation of matrix inversion.
## For applications where some computations are repeated this solution can speed up the execution. 
## The result is cached when first calculated and returned from cache when called again. 
## This guarantees that the inverse of the matrix is computed only ones. 


## This function constructs a cached matrix object and should be used in conjunction with the cacheSolve function. 
## A matrix is given as input.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initiate m
  
  ## Define setter function for indata
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Define getter function for indata
  get <- function() x
  
  ## Define setter function for result
  setinv <- function(solve) m <<- solve
  
  ## Define getter function for result
  getinv <- function() m
  
  ## Make cached matrix object and return it.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function takes a cache matrix object as input and returns the invers of the matrix. 
## If the invers is not yet calculated the inversion is performed and the result returned. 
## When the inversion has been calculated in a previous call the result is returned immediately. 
cacheSolve <- function(x, ...) {
  ## Get invers from cache matrix      
  m <- x$getinv()
  
  ## Check if value is calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## return cached value and exit
  }
  
  data <- x$get() ## Get indata from cache
  m <- solve(data, ...) ## Compute inverse
  x$setinv(m) ## Return a matrix that is the inverse of 'x'
}

## 
## Test of 2x2 matrix inversion
## Comparison should return a 2x2 matrix with TRUE values and the actual result below.
##
m1 <- matrix(c(1,2,3,4),2,2)
mc1 <- makeCacheMatrix(m1)
a1 <- cacheSolve(mc1)
a1 == solve(m1)
a1 <- cacheSolve(mc1)
print(a1)
## Expected result
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## 
## Test of 4x4 matrix inversion
## Comparison should return a 4x4 matrix with TRUE values  and the actual result below.
##
c2 <- c(1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 0, 1, 4, 2, 3 )
m2 <- matrix(c2, 4,4)
mc2 <- makeCacheMatrix(m2)
a2 <- cacheSolve(mc2)
a2 == solve(m2)
a2 <- cacheSolve(mc2)
print(a2)
## Expected result
##      [,1] [,2] [,3] [,4]
## [1,]   -1   -2    3    1
## [2,]    2    1   -3    0
## [3,]    1    1   -1   -1
## [4,]   -1    0    1    0
