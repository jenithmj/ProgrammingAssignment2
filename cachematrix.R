## Assignment here is to write two functions  "makeCacheMatrix" and "cacheSolve" which can cache the inverse of a matrix


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

##Checking the program
##m <- matrix(rnorm(16),4,4)
##m1 <- makeCacheMatrix(m)
##cacheSolve(m1)
##getting cached result from Makecachemartix
##[1,]  3.059502  3.111808  7.646264 -3.922744
##[2,]  2.969226  1.585716  6.121172 -3.195205
##[3,] -4.490204 -3.998541 -9.434814  5.538661
##[4,]  5.205640  3.663658  9.906760 -5.112904