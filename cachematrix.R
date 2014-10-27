## The first function caches the inverse of a matrix
## The second function caluclates the inverse of the matrix
## Caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

    i  <- NULL
    set  <- function(y){
      x <<- y
      i <<- NULL 
    }
    get  <- function() x
    setinverse  <- function(inverse) i  <<- inverse
    getinverse  <- function() i
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
  }
  
  
  
  ## Returns the inverse of a matrix 'x'
  
  cacheSolve <- function(x, ...) {
    i  <- x$getinverse()
    if (!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data  <- x$get()
    i  <- solve(data, ...)
    x$setinverse(i)
    i
    
}


