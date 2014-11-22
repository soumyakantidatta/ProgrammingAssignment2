## Functions to cache inverse of matrix

makeCacheMatrix <- function(mat = matrix()) {
    inv<-NULL
	set<-function(matrix){
	    mat<<-matrix
		inv<<-null
	}
	get<-function() mat
	
	setinverse<-function(inverse) {
	    inv<<-inverse
	}
	
	getinverse<- function() inv
	
	list(set= set,get= get, setinverse=setinverse, getinverse=getinverse)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mat<-x$getinverse()
	
	if(!is.null(mat)){
	    message("getting cached data")
		return(mat)
	}
	data<-x$get()
	mat<-solve(data, ...)
	
	x$setinverse(mat)
	
	mat
}
