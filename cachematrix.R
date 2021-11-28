## makeCacheMatrix: 
# This function creates a kind of imaginary bucket that holds a matrix.
# It can store the contents of this bucket as well as storing the inverse of the content of the bucket in...
# a kind of imaginary shed where we store the imaginary buckets that have the matrices and their inverses.

makeCacheMatrix <- function(x=matrix()) {
        
        inv.mat <- NULL
        set.mat <- function(y) {
                x <<- y
                inv.mat <<- NULL
        }
        get.mat <- function () x
        set.inv <- function(inverse) inv.mat <<- inverse
        get.inv <- function() inv.mat
        list(set.mat = set.mat, 
             get.mat = get.mat,
             set.inv = set.inv,
             get.inv = get.inv)
}

## cacheSolve: 
# This function actually takes one of the buckets with a matrix inside of it and computes the inverse of that matrix.
# Do you remember that shed with the buckets? It may be that we already have the inverse of the matrix sitting in a bucket...
# in our shed (thanks to makeCacheMatrix).
# The function below checks the shed for a bucket with the inverse of the matrix we are working with.
# If the inverse is already in their we get a nice message saying "getting cached data" and the contents of the bucket ...
# are dumped onto our screen. On the other hand, if the inverse matrix bucket is empty then the function grabs the bucket with...
# the un-inversed matrix, inverts it and dumps it on our screen.

cacheSolve <- function(x, ...){
        inv.mat <- x$get.inv()
        if(!is.null(inv.mat)) {
                message("getting cached data")
                return(inv.mat)
        }
        mat.data <- x$get.mat()
        inv.mat <- solve(mat.data,...)
        x$set.inv(inv.mat)
        inv.mat 
}
## setting up the test for cacheSolve below...
A <- matrix(1:4, 2, 2)
A1 <- makeCacheMatrix(M)

## checking my results
cacheSolve(A1) # run it once

## checking for that sweet, sweet message
cacheSolve(A1) # Run it twice
