## A pair of functions to return the inverse of a matrix.

##  The makeCacheMatrix function creates a list containing functions to :
##	1. set value of matrix.
##	2. get value of matrix.
##	3. set value of inverse matrix.
##	4. get value of inverse matrix.
##
makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) inversedMatrix <<- inverse
        getMatrix <- function() inversedMatrix
	list(set = set, get = get, setMatrix= setMatrix, getMatrix = getMatrix)
}

## The cacheSolve calculates the inverse of a given matrix using solve function.
## It checks whether the inverse has been calculated, and retrieves the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedMatrix <- x$getMatrix()
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }
        data <- x$get()
        inversedMatrix <- solve(data)
        x$setMatrix(inversedMatrix)
        inversedMatrix
}

## Sample Test (Create a matrix of 1000x1000)

##
## t1<-round(runif(1000000,1,1000))
## t2<-matrix(t1,nrow=1000,ncol=1000)
## tempC <- NULL
## tempM <- NULL
## startT <- proc.time()
## tempM <- makeCacheMatrix(t2)
## tempC <- cacheSolve(tempM)
## proc.time() - startT
##
## startT <- proc.time()
## tempC <- cacheSolve(tempM)
## proc.time() - startT
##
##
## Sample Output:
##
## > t1<-round(runif(1000000,1,1000))
## > t2<-matrix(t1,nrow=1000,ncol=1000)
## > tempC <- NULL
## > tempM <- NULL
## > startT <- proc.time()
## > tempM <- makeCacheMatrix(t2)
## > tempC <- cacheSolve(tempM)
## > proc.time() - startT
##    user  system elapsed 
##    1.46    0.00    1.46 
## > 
## > startT <- proc.time()
## > tempC <- cacheSolve(tempM)
## getting cached data
## > proc.time() - startT
##    user  system elapsed 
##       0       0       0 
