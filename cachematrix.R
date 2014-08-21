
##My two functions serve to store a matrix and cache its' computed inversion. 
## If the inversion is not computed it is firstly returned and then cached. 
## In order to save the "energy" of the programme the cached inversion if needed 
## may be returned from the memory later without computation by using the function cacheSolve().


## This function is used to store a matrix and a list of functions needed to compute the matrix' inversion.
## This function does not compute the inversion of a matrix. Instead it creates a list of functions
## which are executed via cacheSolve function. They are: setinverse, getinverse, set, get. 
## setinverse is executed when you run cacheSolve first time. Getinverse will return the cached inversion
## in the subsequent use of the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL;
  set<-function(y){x<<-y 
                   i<<-NULL}
  get<-function() {x}
  setinverse<-function(solve) i<<-solve
  getinverse<-function() i
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)}


## This function serves to calculate the inversion of the matrix stored in the makeCacheMatrix function
## and execute the functions which are also stored in the previous function. If the inversion has  not 
## been yet calculated (i=NULL) the function calculates it and cache. If the inversion has been already
## stored the function returns the message "getting cached data" and the result of the inversion of the
## matrix.

cacheSolve <- function(x, ...) { 
  i<-x$getinverse()
  if(!is.null(i)){message("getting cached data") 
  return(i)}
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
  }
