## this function speeds up a potentially time consuming
## computation of inversing a matrix by caching the inversed values. 
## The steps involved are:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

## makeCacheMatrix - inverse a matrix using caching for performance

makeCacheMatrix <- function(x = matrix()) { 
     m <- NULL
    
     set <- function(y) { 
         x <<- y 
         m <<- NULL 
     } 
     
     get <- function() x 
 
     
     setinverse <- function(inverse) m <<- inverse 
     
     
     list(set=set, get=get, 
	setinverse=setinverse, 
	getinverse=getinverse) 
} 


## This function calculates the inverse of the special "matrix" created with the function makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function

cacheSolve <- function(x, ...) { 
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse() 
     if(!is.null(m)) { 
         message("getting cached data.") 
         return(m) 
     } 
     data <- x$get() 
     m <- solve(data) 
     x$setinverse(m) 
     m
} 
