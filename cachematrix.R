## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
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


## Write a short comment describing this function

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
