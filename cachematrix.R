# The purpose of this file is to find the inverse of an invertible 
# matrix and if the inverse already exists, the inverse will be called
# from a cashe, not calculated again saving valuable time. 

makeCasheMatrix <- function(x = matrix()){
  # note: we assume for this function all matrices are invertible
  # this is the parent function
  inv <- NULL
  #set value of the matrix
  set <- function(y){
    # notice the double arrow, this is a variable declared 
    # within an inner function
    x <<- y
    inv <<- NULL
  }
  # get value of the matrix outside the set function
  get <- function() {x}
  # take inverse of the matrix
  setInverse <- function(inverse) {inv <<- inverse}
  # get the value of the inverse
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

casheSolve <- function(x,...) {
  # this function returns a matrix that is the inverse of 'x'
  # assigns the inverse to the call
  inv <- x$getInverse()
  #  checks if inverse was already calculated
  if(!is.null(inv)){
    # helpful message to user so they know inverse was already in cashe
    message("inverse already calculated, retrieving cashed inverse")
    return(inv)
  }
  # inverse not calculated yet, so calculate and return to user
  mat <- x$get()
  # solve is a standard R function
  # gould use the MSSS::ginv() function as an alternative
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}



