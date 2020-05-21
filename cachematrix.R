## Assume all the matrix given are invertible.


## makeCacheMatrix creates special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ##defining the special "matrix"
  j <- NULL     ##j holds value of matrix inverse 
  set <- function(y){
    x <<- y     ##value of matrix in parent environment
    j <<- NULL  ##new matrix , we have to reset the value of j
  }
  get <- function()x   ##this functions returns value of matrix argument
  
  setInverse <- function(inverse) j <<- inverse  ## assigns value of j in parent environment
  getInverse <- function() j                     ## gets the value of j where called
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  #return the inverse if it s already calculated
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  #get the matrix from the object
  mat <- x$get()
  j <- solve(mat,...)
  #set the inverse to our object
  x$setInverse(j)
  j
}
