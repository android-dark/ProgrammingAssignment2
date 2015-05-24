## Put comments here that give an overall description of what your
## functions do

## Creates a "special" matrix object that contains a matrix and the inverse of that matrix, with get
## and set methods for both values.

makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL     #creates a variable for the inverse of a matrix, initialized to NULL
  
  set <- function(y){    #sets the value of the matrix
    x <- y
    inv <- NULL          # reinitialize inv to NULL
  }
  
  get <- function() {x}  #gets the value of the matrix
  
  setinv <- function(i) {inv <- i}   #sets the value of the matrix inverse
  
  getinv <- function() {inv}   #gets the value of the matrix inverse
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)  #lists the methods of the matrix object
  
}


## Takes a "special" matrix object, returns the inverse of that matrix, and caches that inverse within
## the matrix object

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()  #create a variable for the inverse matrix and set its value to the value of 
                     #the input matrix's inverse
  
  if (!is.null(inv)) {  #check if the value of inv is not NULL then returns the inverse matrix
    return(inv)
  }
  
  data <- x$get()   #copy the matrix value
  
  inv <- solve(data, ...) #inv is now the value of the inverse of data
  
  x$setinv(inv)  #cache the inverse matrix into the input matrix object
  
  inv  #returns the inverse matrix
  
}
