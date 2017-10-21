

makeCacheMatrix <- function(x = numeric()) {
  
  m <- NULL;
  set <- function(y) {    #set the x in cache
    x <<- y;
    m <<- NULL;
  }
  
  get <- function() { return(x); }   #function to get the x from makeCacheMatrix() function parameter x, or the x in cache
  
  setMatrix <- function(matrix) { m <<- matrix; }   #function to set the m in cache
  getMatrix <- function() { return(m); } #function to get the m in cache
  
  
  return(list(set = set, get = get, setmatrix=setMatrix, getmatrix=getMatrix)); #return a list containing all the functions
  
}

cacheSolve <- function(x, ...) {
  #x is the list(set = set, get = get, setmatrix=setMatrix, getmatrix=getMatrix) in makeCacheMatrix
  
  m <- x$getmatrix();   #call the getMatrix() function to get the m stored in cache
  if(!is.null(m)) {  #if m is not empty, just return m
    message("get chached data");
    return(m);
  }
  
  matrix <- x$get();    #if m is empty, call the get function to get the x passed into makeCacheMatrix(x)
  m <- solve(matrix, ...);  #solve the x passed into makeCacheMatrix(x)
  
  x$setmatrix(m);  #call the setMatrix() function to set the m stored in cache to the solved results
  
  return(m);  #return the solved results
}


##Testing
A <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)

##Normal Testing
message("Testing Small Matrix...")
message("Input Matrix: ")
A

message("Testing with cache")
x1 <- makeCacheMatrix(A)
x2 <- cacheSolve(x1)
x2

message("Testing without cache")
x3 <- solve(A)
x3

message("Results are identical: ")
identical(x2, x3)


##BIG MATRIX Testing
message("Testing Large Matrix...")
B <- matrix(rnorm(2500), nrow=50, ncol=50)
message("Input Matrix: ")
B

message("Testing with cache")
x1 <- makeCacheMatrix(B)
x2 <- cacheSolve(x1)
x2

message("Testing without cache")
x3 <- solve(B)
x3

message("Results are identical: ")
identical(x2, x3)
