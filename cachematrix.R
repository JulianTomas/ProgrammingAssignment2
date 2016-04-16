## Caching the inverse of a matrix
## Repeatedly calculating the inverse of a matrix can be costly, so caching could help reduce costly re(calculation)
## Below are two functions: makeCacheMatrix and cacheSolve--which 1. create a special object than can cache
## its inverse (makeCacheMatrix) and 2. compute the inverse of the matrix created (cacheSolve)

## Below is the first function (makeCacheMatrix) which creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
        x<<-y
        inv<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## Outlined below is the second function (cacheSolve) which computes the inverse of the special "matrix".  if the inverse has already
## been calculated (and the matrix hasn't changed), the cacheSolve function will retrieve the inverse from the cache created by (makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)) {
              message("retrieving cached data")
              return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
