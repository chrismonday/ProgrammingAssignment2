## Here is the HW for  special matrix-- caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    
    m <<- NULL # Clear  cache
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  
  getInverse <- function() m
  
  # Return  list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#'  inverse of matrix x

cacheSolve <- function(x) {
  m <- x$getInverse() 
  if(!is.null(m)) { 
    message("finding info")
    return(m)
  }
  
  data <- x$get()  # find value of matrix
  m <- solve(data) 
  x$setInverse(m)  
  m                
}

