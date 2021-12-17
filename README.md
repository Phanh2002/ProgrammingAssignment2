## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping


makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

> source("CacheMatrix.R")
> m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
> m1
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
> I2
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
> n1
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> m1%*%n1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> m1%*%n1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> n1%*%m1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> solve(m1)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> myMatrix_object <- makeCachematrix(m1)
> myMatrix_object$get()
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> cacheSolve(myMatrix_object)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
