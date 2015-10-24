## The function calculate and save the inverse of a matrix to avoid
## repeating calculation when users want to get the inverse from
## a previous matrix.

## The firs function creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinver=getinv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse
## has already been caclulated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the matrix 
## inverse and sets the value of the inverse in the cache via the 
## 'setinv' function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
