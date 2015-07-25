# this function creates a list
# contains 4 functions: set, get, setInv and getInv.
#  it uses <<- assignment operator so that

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # this is where the result of inversion is stored
  # A set, use this to set a matrix to object created by makeCacheMatrix function
  # e.g makeCacheMatrix(test1) # here we work on testmatrix
  # makeCacheMatrix$set(test2) # here we work on testmatrix1
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  # return a list that contains these functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line "xinv <- NULL"
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) #  solve it
  x$setInv(m) # set it to the object
  m # return the solved result
}

# Test
# generate a random square, non-singular matrix
test <- matrix(runif(9,1,100),3,3)
# generate the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

testInv <- cacheSolve(testCached)
