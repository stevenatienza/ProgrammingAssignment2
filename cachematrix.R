## Steven Atienza
## November 16, 2018
## This function can cache the inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.
##Note: It is given that this all matrix that will be feed is invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
         x <<- y
         inv <<- NULL
        }
  
  get <- function() x
  
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  if(!is.null(inv)){
        message("getting cached data")
         return(inv)
                   }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setInverse(inv)
  
  inv      
}

##Program  Validation:
## Input this Matrix on console
##  A = matrix (
##  c(3, 0, 2, 2, 0, -2, 0, 1, 1),
##  nrow=3,
##  ncol=3,
##  byrow=TRUE
##  )
##++++++++++++++++++++++++++++++++++
## A1 <- makeCacheMatrix(A)
## cacheSolve(A1)
##        [,1] [,2] [,3]
##  [1,]  0.2  0.2    0
##  [2,] -0.2  0.3    1
##  [3,]  0.2 -0.3    0
## Same Values Computed appered on this manually computed inverse of Matrix
##https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html

