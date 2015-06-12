## Author:              Krish Suri.
## Date:                June 11th, 2015.
## Modified date:       June 12th, 2015
## Description:         The following functions create and cache a matrix and its inverse. Also,
##                      compute its inverse if it does not exist already and 
##                      set it in cache for subsequent usage.


## makeCacheMatrix function: 
##      Sets (in cache) or gets a special matrix. 
##      It also sets (in cache) or gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
        ## Initialize inverse matrix to NA
        inverseMatrix <- matrix()
        
        ## Retrieve special matrix(i.e input matrix)
        get<- function()
        {
         x       
        }
        ## Set special matrix (i.e "x") and 
        ## also clean-up inverse matrix in cache if "x" is reset
        set<-function(specialMatrix)
        {
                x <<- specialMatrix
                inverseMatrix <<- matrix()
        }
        ## Retrieve inverse matrix of "x"
        getInverse <- function()
        {
                inverseMatrix  
        }
        ## Set inverse matrix in cache
        setInverse<- function(invMatrix)
        {
                inverseMatrix <<-  invMatrix
        }
        list(get=get,
             set=set,
             getInverse=getInverse,
             setInverse=setInverse)

}


## cacheSolve function:
##      Checks to see if inverse of given matrix exists. If yes, it is retrieved
##      If not, computes the inverse of the matrix and sets it in cache.

cacheSolve <- function(x, ...) 
{
        ## Get inverse matrix
        inverseMatrix <- x$getInverse()
        ## Check to see if any element in above inverseMatrix is NA
        if(!all(is.na(inverseMatrix)))
        {
                ## If not, retrieve it from cache
                print("Retrieving inverse matrix from cache")
                ## Return the value and quit the function
                return(inverseMatrix)
        }
        ## If yes, i.e NA exists then get special matrix
        mat <- x$get()
        ## Compute inverse using solve() function
        inverseMatrix <- solve(mat)
        ## Set it in cache
        x$setInverse(inverseMatrix)
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix
}
