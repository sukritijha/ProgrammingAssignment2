## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
##a- sets the value of  matrix
##b- gets the value of  matrix
##c- sets the inverse of matrix
##d- gets the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## This function first checks if the inverse has already been calculated . If yes
## it simply returns the result or else if not then it first calculates the inverse of the
## matrix and sets the value in the cache using setinverse function.
# this function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("fetching cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
