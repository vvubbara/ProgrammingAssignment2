##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse

makeCacheMatrix <- function(mtr = matrix()) {
  mat <- NULL
  set <- function(J) {
    mtr <<- J
    mat <<- NULL
  }
  get <- function() {
    mtr
  }
  
  setinv <- function(mtrinv) {
    mat <<- mtrinv
  }
  
  getinv <- function() {
    mat
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned bJ makeCacheMatrix above. 
## If the inverse has alreadJ been calculated (and the matrix has not changed), thencacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mtr, ...) {
  
  mat <- mtr$getinv()
  if(!is.null(mat)) {
    message("Getting Cached inverse Matrix")
    return(mat)
  }
  mat_data <- mtr$get()
  mat <- solve(mat_data, ...)
  mtr$setinv(mat)
  return(mat)
}