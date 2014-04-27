## Functions to make a Cache of the matrix and it's inverse 
## one more function to pull the Matrix Inverse from cache if the cache exits 
## No need to calculate the same vaule again and again if the value is cached

## makeCacheMatrix sets up a matrix for creating the inverse and caching the value 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                
  set <- function(y) {
    x <<- y                             #Assign the input to the X's cache 
    m <<- NULL
  }
  get <- function() x                   #Set the vairable get to the passed value
  setinv <- function(solve) m <<- solve #Compute the inverse 
  getinv <- function() m                #get the inverse  
  list(set = set, get = get,            #return the vector of setinv and get inv variables
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve functions returns the cached value if the cache exists 
##it also  puts the value into cache if it doesn't exist in the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()             #pull the x vector's cache  
  if(!is.null(m)) {           #if there is a cache pull the value from cache no need of computation
    message("getting cached data")
    return(m)
  }
  data <- x$get()             #If there is no chace then compute 
  m <- solve(data, ...)
  x$setinv(m)                 #save it to cache again
  m                           #return the result which is the inverse of x
 
}
