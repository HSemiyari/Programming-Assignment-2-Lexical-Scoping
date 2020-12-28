# Programming-Assignment-2-Lexical-Scoping
Programming Assignment 2: Lexical Scoping
## I kept the input x as the matrix function, 
## then set the solved value "j" as a null as it came in the suggested solution
## Finally I changed every reference from "mean" to "solve"


makeCacheMatrix <- function(x = matrix(sample(1:50,9),3,3)) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) j <<- solve
  getsolve <- function() j
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##I continued solving by puting solve instead mean

cacheSolve <- function(x, ...) {
  j <- x$getsolve()
  if(!is.null(j)) {
    message("getting inversed matrix")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setsolve(j)
  j
}
