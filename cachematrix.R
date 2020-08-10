## Put comments here that give an overall description of what your
## functions do

# "makeCacheMatrix()" takes in the matrix for which the inverse needs to be
# calculated and returns a list with the functions to set and retrieve the 
# input matrix and the inverse matrix. This function creates the object that
# is used to set and get the input and inverse matrices.

# "cacheSolve()" takes the object created by "makeCacheMatrix()" and calculates
# the inverse of the input matrix and caches it.



## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- matrix()
  set_mat <- function(mat){
    x <<- mat
    inv_matrix <<- matrix()
  } 
  get_mat <- function() x
  set_inverse <- function(inv_m) inv_matrix <<- inv_m
  get_inverse <- function() inv_matrix
  list(set_mat = set_mat, 
       get_mat = get_mat, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated then 
#cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv_m <- x$get_inverse()
  
  if(!all(is.na(inv_m))){ #Checks if the inverse has already been calculated
    message("Cache found!!! \nFetching cached inverse matrix \n")
    return(inv_m)
  }
  inv_m <- solve(x$get_mat())
  x$set_inverse(inv_m)
  inv_m
}
