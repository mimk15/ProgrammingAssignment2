### Assignment #2 #### 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  n <- NULL 
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  get <- function() x 
  setInv <- function(inv) n <<- inv 
  getInv <- function() n 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) 
    message("gathering cache")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}
