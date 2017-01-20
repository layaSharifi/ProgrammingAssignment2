
## makeCacheMatrix creates a matrix which includes of running four functions :set, get, setInvers,get Invers 
## set and setInverse stores the matrix in cache and in original matrix invers while , get and getInverse recalls the matrix  
## its inverse.  



makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()             #check x in matrix'cach
  if (!is.null(inv)) {              #check whetehte there is a cache that its,inverse has been previously calculated 
    message("getting cached data")  #sent message that this is just cache
    return(inv)                     #return the cache 
  }
  mat <- x$get()                    #useding makeCacheMatrix function to get the matrix
  inv <- solve(mat, ...)            #computing its reverse
  x$setInverse(inv)                 #store the inverse matrix in cache using the makeCacheMatrix set function 
  
}


