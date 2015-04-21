# A.Briano - 2015-04-21
# This function works like a class, it creates a list
# that contains 4 member functions:
# set - get - setInv - getInv 
# using <<- assignment operator to not expose outside
# these internal variables


makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL 				# this is where the result of inversion is stored
  # A setter function, use this to set a matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  # return a list that contains these functions, so that we can use
  # makeCacheMatrix object like these
  # x <- makeCacheMatrix(testmatrix)
  # x$set(newmatrix) # to change matrix
  # x$get # to get the setted matrix
  # x$setInv # to set the inversed matrix
  # x$getInv # to get the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m # return the solved result
}

# Prova
# generate a random square, non-singular matrix

## Prova <- matrix(runif(81,9,100),3,3)
# generate the makeCacheMatrix object with this matrix

##ProvaCached <- makeCacheMatrix(Prova)

# from now on calculate or retrieve calculated inversion using the cacheSolve function

## ProvaInv <- cacheSolve(ProvaCached)
