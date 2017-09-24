## Assignment here is to write two functions  "makeCacheMatrix" and "cacheSolve" which can cache the inverse of a matrix.


## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
     inver <- NULL
     set <- function(y) 
       {
         x <<- y
         inver <<- NULL
       }
    
     get <- function() x
     setinver <- function(inverse) inver <<- inverse
     getinver <- function() inver
     list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## This function calculates the inverse of the matrix returned by above function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinver()
    
    if(!is.null(inver)) 
      
      {
        message("getting cached result from Makecachemartix")
        return(inver)
      }
    
    data <- x$get()
    inver <- solve(data, ...)
    x$setinver(inver)
    inver
  
}
