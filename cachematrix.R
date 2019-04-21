## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##int inverse matrix
  inv <- NULL
  
  ##set x as y
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## return x 
  get <- function() x
  
  ##set inv 
  setinverse <- function(inverse) inv <<- inverse
  
  ##get inv
  getinverse <- function() inv
  
  ##return a list of function, can be called such as x$get() 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  ##get x
  data <- x$get()
  ##get inverse of x
  inv <- solve(data)
  ##set inverse of x into inv
  x$setinverse(inv)
  inv
  
}
