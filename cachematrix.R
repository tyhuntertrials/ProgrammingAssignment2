## Creates a cache based on a matrix, where the inverse of that matrix can be stored and then inverses that matrix.

## Creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Providing inv variable that will hold inverse matrix
  set <- function (y){ 
    x <<- y   #Sets new value of matrix from parent function
    inv <<- NULL #Sets inv to null when new matrix is present
  }
  get <- function() x #get function returns matrix
  setInverse <- function(inverse) inv <<- inverse #Sets value of inv to inverse of matrix
  getInverse <- function()inv #Retrieves inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) #Establishing list so that values can be retrieved

}


## Returns a matrix that is the inverse of 'X'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #Get inverse and stores in inv
  if(!is.null(inv)){ # if there is a value in inv then message is displayed and inv is returned
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() #Calls upon the get function above
  inv <- solve(mat, ...) #Inputs inverse of matrix into inv
  x$setInverse(inv) #Makes inv value the inverse
  inv #displays inv
}

