makeCacheMatrix <- function(x = matrix()) {
 
  # this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an  Global environment 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  # return: inverse of the original matrix input to makeCacheMatrix()
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and negating the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # Else otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the set function.
  x$setinv(inv)
  
  return(inv)
}