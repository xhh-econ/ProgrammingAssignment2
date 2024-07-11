## Below two functions are used to create a special object that stores 
## a matrix and cache's its inverse matrix.

## The first function makeCacheMatrix creates a special "Vector", a list 
## that contains four elements:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The following function calculates the inverse of the matrix from 
## the special "vector" created above. It checks to see if the 
## inverse matrix has been calculated. If so, it gets the inverse 
## matrix from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the 
## matrix in the cache via the sematrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!all(is.na(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m

}
