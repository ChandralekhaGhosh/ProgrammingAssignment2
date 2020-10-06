## Put comments here that give an overall description of what your
## functions do

#In this assignment we're writing a function to cache the inverse of a square matrix.
#Caching is an efficient way to avoid re-eval long intensive computations.

## Write a short comment describing this function

#makeCacheMatrix:creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL             ##Initializing values
  set<-function(y){     ## set the value of the vector 
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}    ## get the value of the vector
  
  setInverse<-function(inverse){inv<<-inverse} ## set the value of the inverse
  
  getInverse<-function(){inv}  ## get the value of the inverse
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  

}


## Write a short comment describing this function

#cacheSolve calculates inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()   ## Return a matrix that is the inverse of 'x'
 
   #Checking if the inverse already been calculated, if so can get the inverse 
  # from Cache, can skip the computization. A message will be popped up
  
  if(!is.null(inv)){
    message("getting from cache")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)  ##solve:standard R fun to compute inverse of the matrix
  
  x$setInverse(inv)  #set the value of the inverse in the Cache using setInverse 
  inv
}


################CHECKING#######################################################

testmat1<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
testmat1$get()
testmat1$getInverse()
cacheSolve(testmat1)

