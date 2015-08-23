##Functions for fast repetitive calculation of a matrix 
## inverse
 

##function used to return a list which  exposes methods to work with enclosed matrix and its inverse cache
makeCacheMatrix <- function(x = matrix()) {
  #variable to cache inverse   
  i <- NULL
  
  #function to set given matrix to x and reset cache to null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get the matrix enclosed in this entity
  get <- function() x
  
  #set the cache in this entity
  setInverse <- function(inverse) i <<- inverse
  
  #get the cache in this entity for the enclosed matrix
  getInverse <- function() i
  
  #list of methods exposed to work with enclosed matrix and its cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##function used to calculate inverse for list output of makeCacheMatrix and cache it
cacheSolve<- function(x, ...) {
  
  #check if inverse is already cached      
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #inverse not cached,calculating
  data <- x$get()
  i <- solve(data, ...)
  
  #caching for further use
  x$setInverse(i)
  
  #return
  i
}
