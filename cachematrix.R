## makeCacheMatrix creates a special matrix object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The function make cache matrix does the following things :
##            1. Set the value of matrix    (by set function).
##            2. Get the value of matrix    (by get function).
##            3. Set the inverse of matrix  (by setInverse function).
##            4. Get the inverse of matrix  (by getInverse function).

makeCacheMatrix <- function(x = matrix())
{
   i = NULL
   set <- function(y)
      {
         x <<- y 
         i <<-  NULL
      }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse   
  getInverse <- function() i
  
  list(set=set,get=get,setInverse=setInverse,
       getInverse=getInverse)
## returns a list of above functions
}


## The function cacheSolve returns the inverse of matrix returned by above 
## function but first it checks if inverse has already been calculated/saved
## if not then it solves the value.

cacheSolve <- function(x, ...) 
{
    i <-    x$getInverse()
    if(!is.null(i))
      {
        message("getting cache of inverse of matrix")
        return(i)
      }
    
     matrix = x$get()
     i = solve(matrix,...)
     x$setInverse(i)
     return(i)
  ## Return a matrix that is the inverse of 'x'
}
