## The Project is the second programming assignment on course
## R Programming started on May, 2014.
##
## The aim is to cache heavy matrix calculations using scoping
## rules of R

## This function creates a list with cachable matrix object and
## methods to manipulate this object

## Example of use:
## > x <- matrix(c(1,2,3,4),nrow=2,ncol=2)
## > xcache <- makeCacheMatrix(x)
## > y <- cacheSolve(xcache)
## > y <- cacheSolve(xcache)
##    getting cached inverse
## > x %*% y ## must return identity matrix..
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## .. so it is!

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## This is a cached value
  set <- function(y) { ## The function to set new value to cachable matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x # Getter method
  setinverse <- function(minv) m <<- minv ## Setting inverse cache
  getinverse <- function() m ## Gettinge cached inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) ## Creating an object with getters and setters
}


## The function returns cached inverse matrix if it exists
## and calculates it vice versa

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) { ## if there is a chached value...
    message("getting cached inverse")
    return(m) ## ..return it
  } ## ..otherwise get data, calculate inverse and cache
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
