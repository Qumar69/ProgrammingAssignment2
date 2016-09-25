## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

#initiate the variable 
		 m <- NULL
		 #set vale of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		#get value of matrix
        get <- function() x
		#set inverse value of matrix
        setInverse <- function(inverse) m <<- inverse
        #get inversea value of matrix
		getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#get inverse value of matrix
		m <- x$getInverse()
		#check if matrix is null, if not null return value of matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		#get matrix data
        data <- x$get()
		#inverse the matrix
        m <- solve(data, ...)
#set the inverse matrix to the return value
        x$setInverse(m)
        m
		
		}