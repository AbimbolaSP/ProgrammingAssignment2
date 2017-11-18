
## Two functions - makeCacheMatrix & cacheSolve - are created to compute and  
## cache the inverse of a matrix when the matrix is recurring.

## The first function creates an object that can cache the inverse of a matrix:
## sets the content of the matrix
## gets the content of the matrix
## sets the inverse 
## gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse, 
       getinverse=getinverse)
}


## The second function calculates the inverse of the matrix called above, if non-existent, 
## otherwise retrieves cached inverse and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("get cached data")
    return(x)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)        
  i                        ## Returns a matrix that is the inverse of 'x'
}