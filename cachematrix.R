## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## set matrix inverse to NULL
  MI <- NULL
  
  ## create matirx
  set <- function(y) {
    x <<- y
    MI <<- NULL
  }
  
  ## get matrix value
  get <- function() x
  
  ## set matrix inverse
  setverse <- function(verse) MI <<- verse
  
  ## get matrix inverse
  getverse <- function() MI
  
  ## list functions of of makeCacheMatrix 
  list(set = set, get = get,
       setverse = setverse,
       getverse = getverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## assign input matrix
  MI <- x$getverse()
  
  ## If inverse value exists, return inverse matrix
  if(!is.null(MI)) {
    message("getting cached matrix")
    return(MI)
  }
  
  ## If there is no cached value then derive inverse matrix by using makeCacheMatrix 
  else {
    data <- x$get()
    MI <- solve(data, ...)
    x$setverse(MI)
    MI
  } 
}
