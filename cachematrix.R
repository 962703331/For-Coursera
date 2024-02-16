## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are two functions makeCacheMatrix, makeCachMatrix
##makeCachMatrix consists of set,get,setinv, getinv
##library(MASS) is used yo calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
                   }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%%x
                    }
  list(set, get = get,
       setinv = setinv,
       getinv = getinv)
}  
## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
        
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv       ## Return a matrix that is the inverse of 'x'
   }