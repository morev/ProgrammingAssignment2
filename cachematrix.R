## Put comments here that give an overall description of what your
## functions do
#  The purpose of these R functions is to help speed up the 
#  time required to compute a matrix inverse in case if we 
#  have already computed it before using a caching mechanism to 
#  store previously computed inverse data for each matrix 

# the assumption is we can compute the inverse of the given matrix.
# ADDITIONAL CAHNGES : 
#     Still the code tries to validate this assumption and 
# prints a message saying the given matrix can be inversed.

# for eg  if we have a matrix(R) of say 
# 2000 * 2000 dimensions 
# and running solve on it takes around 8 seconds 
# then using our mechanism 
# mymet1 <- makeCacheMatrix(R)
# cacheSolve(mymet1)      will take say 8 seconds
# BUT if i calc it again 
# cacheSolve(mymet1)      this should reuse the existing value
# and  should take almost no time (0 seconds)


## Write a short comment describing this function
# this function converts a regular matrix into our defination 
# of the CacheMatrix along with its functions to provide 
# the associated inverse if calculated
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x 
  
  #set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  
  #get the value of the inverse
  getinverse <- function() m
  
  #expose the internal functions i think
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #first check if we have ever found  out the mean for this 
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse data")
    return(m)
  }
  #finding the mean for this for the first time
  data <- x$get()
  #check if this matrix can be inverted 
  if (det(data) == 0) {
    message("NOTE:  The passed matrix CANNOT BE INVERTED!!")
    message("       aborting...")
    return()
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
