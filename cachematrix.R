## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates 4 functions that later are added in a list. It has 1 input parameter 
## which is a matrix and is used as an input parameter of "get", 1 of the 4 functions stored in the list. Above each
## of the function's declaration there is a short description about what is it used for.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  #function that re-writes the matrix passed as an input parameter
  set <- function(y){
    x <<-y
    m <<-NULL
  }
  #function get returns input user
  get <- function()x
  #funtion setmean, the user sets the inverse matrix 
  set_inv <- function(inv = matrix()) inv <<- inv
  #function get_inv, return the inverse of the matrix
  get_inv <- function() inv
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  

}


## cacheSolve tries to avoid calculating the inverse of the matrix that is stored in the list returned in makeCacheMatrix
## first looking if it was computed (through the "set" function of the list in makeCacheMatrix) and in case it was not,
## calculating it and storing it (through the "set_inv" function of the list in makeCacheMatrix) for later retrievals

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #returns the inverse matrix in case it exists
  inv <- x$get_inv()
  #... in case it already exists...
  if(!is.null(inv)){
    message("Hey! this was already computed before!!")
    return(inv)
  }
  #... in case it does not exists...
  data <- x$get()
  #... we calculate it...
  inv <- solve(data, ...)
  #... and we add its value to the list defined in makeCacheMatrix
  x$set_inv(inv)
  #... and of course we return it!
}
