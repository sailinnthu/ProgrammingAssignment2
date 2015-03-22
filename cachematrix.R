## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL                                      ## assign 'NULL' to inv
  set <- function(y)                               ## set the matrix 'x'
    {                              
          x <<- y
          inv <<- NULL
    }
  get <- function()                                ## return the matrix 'x'
    {
          x
    }
  setinv <- function(i)                            ## setinv sets the inv variable
    {
          inv <<- i
    }
  getinv <- function()                             ## getinv gets the cached inverse
    {
          inv
    }
  
  list(set = set,                                  ## return the special matrix
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        inv <- x$getinv()                         ## get the cached inverse
        
        if(!is.null(inv))                         ## if the inverse is actually cached, just return it
        {
          message("getting cached inverse")
          return(inv)
        }
        
        matr <- x$get()                           ## otherwise, calculate the inverse and cache it
        inv <- solve(matr, ...)
        x$setinv(inv)
        
        return(inv)
}
