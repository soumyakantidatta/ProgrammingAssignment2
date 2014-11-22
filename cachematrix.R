## Functions to cache inverse of matrix

## Function to set and retrieve the matrix and its inverse
makeCacheMatrix <- function(mat = matrix()) {
    
	#initialize inverse
	inv<-NULL
	
	#set matrix
	set<-function(matrix){
	    mat<<-matrix
		inv<<-null
	}
	
	#get matrix
	get<-function() mat
	
	#set inverse
	setinverse<-function(inverse) {
	    inv<<-inverse
	}
	
	#get inverse
	getinverse<- function() inv
	
	#return method list
	list(set= set,get= get, setinverse=setinverse, getinverse=getinverse)
}



##Computes the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mat<-x$getinverse()
	
	if(!is.null(mat)){
	    message("getting cached data")
		return(mat)
	}
	# obtain matrix
	data<-x$get()
	
	#compute inverse
	mat<-solve(data, ...)
	
	#set inverse
	x$setinverse(mat)
	
	#return matrix
	mat
}
