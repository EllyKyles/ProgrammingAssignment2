## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below is to get a matrix as an input, set and get the value
## of the matrix and set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function () x
  setinverse <- function (inverse) inv <<- inverse
  getinverse <- function () inv
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## cachesolve funtion is to take the output of the privious funtion called 
## makeCacheMatrix and to compute the inverse of the output using solve funtion 
## when inverse matrix from makeCachemake funtion is empty. if not empty, 
## it returns the message saying "getting cached data".


cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        message("getting Cached data")
        return (inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        return (inv)
          
}

## note that this technique allows use or $ operator to access
## each function from the list
