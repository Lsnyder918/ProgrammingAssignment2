## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                ##sets value of x
  m <- NULL    ##Nulls value of m
 
   set <- function(y) {
    
     x <<- y     ##assigns input argument to parent environment
    
     m <<- NULL  ##clears any value of m that has been previously cached   
  } 
  
   get <- function()x     ##lexical scoping/since x is not defined, retreives from parent environment
 
   setinverse <- function(inverse) m <<- inverse ##assigns input to m in the parent environment
 
   getinverse <- function () m ##lexical scoping to find the value of m
 
   list(set = set, get = get, ## gives the name to the function of the same name above in the parent environment
     
       setinverse = setinverse, ##elementname = value
     
       getinverse = getinverse)
}


## sets and caches the matrix

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() ##calls the getinverse function
  
  if(!is.null(m)){   ##checks to see whether the value is NULL
    
      message("getting cached data")
    
      return(m) ##if not NULL, returns m value to the parent environment
  }
  
  data <- x$get() ## if false, it gets the data from the input object
  
  m <- solve(data, ...) ##calculates the inverse
  
  x$setinverse(m) ##sets the inverse to the input object
  
  m  ##returns value of the inverse to the parent environment
}
