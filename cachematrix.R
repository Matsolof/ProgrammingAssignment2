##  With these two functions any invertible matrix may be
##  inverted and the inverted result cashed


##  This function (object?) creates an instance of a certain
##  matrix and its inverse (by calling casheSolve)

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv<<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv<- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


##  This function calculates the inverse of a given matrix
##  after first checking for its existense in the cashe
##  in case it is delivered directly from there

cacheSolve<- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}

amatrix<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2, ncol=2))

amatrix$get()

cacheSolve(amatrix)