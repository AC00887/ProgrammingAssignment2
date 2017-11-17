## Put comments here that give an overall description of what your
## functions do

##Write a short comment describing this function

#This functiom makeCahceMatrix gets a matrix as an input, set the value of the matrix, 
#get the value of the matrix,
#set the inverse Matrix and get the inverse Matrix. The matrix object cache is its own object.

#<<- operator is used to assign a value to an object in an enviroment that is different from
#the current enviroment

makeCacheMatrix <- function(x = matrix()) { 
   invMatrix <- NULL
        
#set the value of the Matrix
  setMatrix <- function(y)  { 
    x <<- y 
    invMatrix <<- NULL
          
    }
        
    
       
getMatrix <- function() x                                #get the value of the Matrix
setInverse <- function(inverse) invMatrix <<- inverse    #set the value of the invertible matrix     
getInverse <- function() invMatrix                       #get the value of the invertible matrix
list(setMatrix = setMatrix, getMatrix = getMatrix, 
     setInvers = setInverse, getInverse = getInverse)

}


#The functiom cacheSolve takes the output of the previous matrix makeCahceMatrix as an input
#and checks inverse matrix from maheCacheMatrix(matrix) has any value in it or not.
#In case inverse matrix from maKeCacheMatrix(matrix) is empty, it gets the original matrix data
#from and set the invertible matrix by sing the solve function.
#In case inverse matrix from maKeCacheMatrix(matrix) has some value in it (always works
#after running the code first time), it return message "Getting Cached Invertible Matrix"
#and get the cached object

            
## Write a short comment describing this function

cacheSolve <- function (x, ...) {
         
#get the value of the invertible matrix from the makeCacheMatrix(matrix) function

        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
          message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
          return(invMatrix)                             #return the invertible matrix
          }            
        
 #if value of the invertible matrix is NULL then
         MatrixData <- x$getMatrix()                    #get the original Matrix Data
         invMatrix <- solve(MatrixData, ...)            #use solve function to inverse matrix
         x$setInverse(invMatrix)                        #set the invertible matrix
        return(invMatrix)
        ##Return a mtrix that is the invesr of "x"
        
 }
