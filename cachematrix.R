##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inverse<-NULL
  set<-function(y)
  {
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) inverse<<- solve
  getmatrix<-function() inverse
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), thencacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  getmat<-x$getmatrix()
  if(!is.null(getmat))
  {
    message("Getting Cached Data")
    return(getmat)
  }
  matrix<-x$get
  getmat <-solve(matrix, ...)
  x$setmatrix(getmat)
  getmat
}
