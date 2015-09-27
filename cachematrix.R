
## Below codes perform caching the inverse of a matrix. It is composed of two functions:
## 1. makeCacheMatrix create a special "matrix" object that can cache its inverse
## 2. cacheSolve computes the inverse of a special "matrix" returned by makeCacheMatrix. 

## The function creates of a list of following functions:
	#1.set the value of the matrix
	#2.get the value of the matrix
	#3.set the value of the inverse
	#4.get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	I<-NULL
	set<-function(y){
		x<<-y
		I<<-NULL
	}
	get<-function()x
	setinverse<-function(solve) I <<-solve
	getinverse<-function() I
	list(set=set, get=get,
	setinverse=setinverse,
	getinverse=getinverse)
}


## Below function compute the inverse of a special "matrix" created with the above function. If first checks to see if inverse has already been calculated.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	I<-x$getinverse()
	if(!is.null((I))){
		message("getting cached data")
		return(I)}
		
	data<-x$get()
	I<-solve(data,...)
	x$setinverse(I)
	I
       ## Return a matrix that is the inverse of 'x'
}
