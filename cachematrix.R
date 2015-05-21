    ## 'makeCacheMatrix' function creates a special "matrix" object 
    ## that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) {
    i <- NULL

    ## now we will set the value of the matrix

    set <- function(y) {
            x <<- y
            i <<- NULL
    }

    ## next step is to get the value of the matrix

    get <- function() x

    ## It is time to set the value of the inverse matrix 

    setinver <- function(solve) i <<- solve

    ## Here we get the value of the inverse matrix 

    getinver <- function() i
    list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}

    ## 'cacheSolve' is a function that calculates the inverse of the 
    ## special "matrix" created with the function "makeCacheMatrix".
 
cacheSolve <- function(x, ...) {

    ## This function first checks to see if the inverse matrix
    ## has already been calculated. 
     
            i <- x$getinver ()
            if(!is.null(i)) {
    
                     message("getting cached data")

    ## If inverse matrix has already been calculated, 
    ## it gets the inverse matrix from the cache and skips the computation.
    ## Return a matrix that is the inverse of 'x'

                     return(i)
            }
            data <- x$get()
            i <- solve(data, ...)
            x$setinver(i)
            i
}
    ## Examples to test the function
  
    ## Example nº1

x = makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
cacheSolve(x)
cacheSolve(x)


    ## Example nº2
    ## You can choose here any consequence of numbers in the matrix.

A = makeCacheMatrix(matrix(scan(), ncol = 2, byrow = TRUE))

cacheSolve(A)
   ## choose any consequence of two 5numbers for rows: 1 and 2. 
   ## When number 5 will appear just press enter to continue.
   ## run again cacheSolve(A) to verify how cachesolve retrieves 
   ## the inverse from the cache.

cacheSolve(A)

   
