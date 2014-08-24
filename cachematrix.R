## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  Inver<-NULL
  set<-function(y){
    x<<-y
    Inver<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) Inver <<- inverse
  getinverse<- function() Inver
  list(set = set, get = get,setinverse =setinverse, getinverse =getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inver<- x$getinverse()
  if(!is.null(Inver)){
    message("getting cached data")
    return(Inver)
  }
  data<-x$get()
  # assume matrix is squre
  n<-nrow(data)
  IM<-diag(n)
  Inver<-solve(data,IM,...)
  x$setinverse(Inver)
  Inver
}
