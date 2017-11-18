
## Two functions - makeCacheMatrix & cacheSolve - are created to compute and  
## cache the inverse of a matrix when the matrix is recurring.

## The first function creates a object that can cache the inverse of a matrix
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


## The second fucntion calculates the inverse of the matrix called above, if non-existent, 
## otherwise retrieves cached inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("get cached data")
    return(x)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)        
  i                        ## Return a matrix that is the inverse of 'x'
}