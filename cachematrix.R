##Calculate the inverse of an invertible square matrix and cache 
## the inverse to avoid recalculation of the inverse (for the same matrix).


## Creates an R object that stores both the matrix A and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set,get = get,setinv = setinv,getinv = getinv)
  
}


## Computes the inverse for the matrix A(input argument should be return value from makeCachematrix)
## getter function getinverse(), retrieves the inverse of matrix 'x'.
## If the inverse is found then return it
## if the inverse is not found(NULL) then compute it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  A <- x$get()
  inv <- solve(A)
  x$setinv(inv)
  inv
}
