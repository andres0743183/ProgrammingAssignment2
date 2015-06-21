

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
               
     inv <- NULL ##stores the inverse of the matrix
      
      ##set the value of the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## get the value of the matrix
      get <- function(){
            x
      }
      
      ##set the value of the inverse
      setinv <- function(inverse){
            inv <<- inverse
      }
      
      
      ##get the value of the mean
      getinv <- function(){
            inv
      }
      ##Returns the functions created in a variable list
      list(set = set, get = get, setinv = setinv, getinv = getinv)  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      
      ##get the value of the inverse and verifies if different from null
      ##returns the value stored in the variable inv and displays 
      ##the message "getting cached data". Function ends 
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ##If the inverse matrix is computed not for the calculated
      data <- x$get() ## Extracts data from makeCacheMatrix
      
      #Determines if is a square matrix
      if(dim(data)[1]!=dim(data)[2]){
            message("It is not a square matrix. Not inverse of matrix")
            return(data)
      }
      
      ##If a square matrix 
      inv <- solve(data, ...) ##Calculate the inverse of the matrix
      x$setinv(inv) ##set the value of the inverse calculation
      inv       ##Return the inverse matrix
}





