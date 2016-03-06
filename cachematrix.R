## sets the value of the matrix,(NULL) gets the value of the matrix,
## sets the value of the inverted matrix,(NULL) gets the value of the inverted matrix.


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    
    x <<- y
    inv <<- NULL
    
  }
    
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## calculates the inverse  of the matrix created in previous function
## if the inverted matrix does not exist, it is created and it's value
##is stored in the case through the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
