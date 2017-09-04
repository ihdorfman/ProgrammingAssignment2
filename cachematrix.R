## The functions contained in this R program will first check to see if 
## the inverse of the submitted matrix has already been calculated then if it
## has will return the cached inverse and if not will return the newly 
## calculated inverse.

## Isaac Dorfman 09032017 - 'ihdorfman' on github.com

## This function calculates, if necessary, the inverse of a supplied matrix and
## caches the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function either calls the base solve function to calculate the inverse
## of a supplied matrix or pulls from cached memory the previous solution.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
                message("Please wait. Retrieving cached results.")
                return (m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

# A simple boolean function to verify if the inverse matrix was calculated 
# correctly using the original matrix, the inverse matrix created with 
# the cacheSolve() function and the 'diag(n)' fuction to create an identity matrix.

verifyMatrix <- function(x,y){
        i <- diag(nrow(x))
        j <- round(x%*%y)
        k <- all.equal(i,j)
        
        if(k==1){
                message("The matrices submitted are inverses!")
        }else{
                message("The matrices submitted are NOT inverses.")
        }
}

# Procedure:
# 1) x <- makeCacheMatrix(*a matrix goes here*)
# 2) y <- cacheSolve(x) #If this is the first time cacheSolve() has been run
#                       #on this particular matrix it may take longer to calculate
# 3) verifyMatrix(*original matrix goes here*, y) 
                        #Implements correct matrix multiplication to verify
                        #that the inverse has been calculated correctly.
                        #Will produce an error if the dimensions of the matrices
                        #submitted do not match.
