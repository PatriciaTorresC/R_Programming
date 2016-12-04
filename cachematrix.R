#Inverse of a Matrix:
#Below is a function that are used to create a special object that 
#stores a matrix and caches its inverse.

## makeCacheMatrix is a function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL                                           #assigning the initial value
  set <- function(y) {                                  #setting the matrix
    x <<- y
    Inv <<- NULL
  }
  get <- function() x                                   #getting the matrix
  setInverse <- function(inverse) Inv <<- inverse       #setting the inverse
  getInverse <- function() Inv                          #getting the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if (!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  ## Test if the matrix has inverse
  mat <- x$get()
  if(det(mat)==0){
    return("The matrix doesn't have determinant. Please change the input.")
  }
  Inv <- solve(mat, ...)                                #getting the inverse
  x$setInverse(Inv)
  Inv
}

## Check the function
x <- matrix(rnorm(9,25,50),3,3)
y <- makeCacheMatrix(x)
cacheSolve(y)
