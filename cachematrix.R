## cacheSolve function accepts a special matrix object return from makeCacheMatrix function, and check whether inverse of the matrix
## provided to makeCacheMatrix function is already calculated. If yes, then return cached inverse matrix, else compute inverse of the ## matrix

## Accepts a Matrix object (mat)
## return a list of below functions
## getMatrix and setMatrix to an input Matrix object
## getInverseMatrix and setInverseMatrix of input Matrix Object using Solve function

makeCacheMatrix <- function(mat = matrix()) {
  
  invMat <- NULL
  setMatrix <- function(y) {
    mat <<- y
    invMat <<- NULL
  }
  getMatrix <- function() mat
  setInverseMatrix <- function(solve) invMat <<- solve
  getInverseMatrix <- function() invMat
  list(getMatrix =getMatrix, setMatrix = setMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}

## Accepts a special object retuned by makeCacheMatrix function
## Return a matrix that is the inverse of a matrix object,input to makeCacheMatrix function

cacheSolve <- function(matObj, ...) {
  
  invMat<- matObj$getInverseMatrix()
  if(!is.null(invMat)) {
    ## getting inverse of the matrix using Cache
    return(invMat)
  }
  ## computing inverse of the input matrix for the first time
  data <- matObj$getMatrix()
  invMat <- solve(data, ...)
  matObj$setInverseMatrix(invMat)
  invMat
}
