##This a function that creates a matrix, sets a matrix, gets the inverse or sets
##the inverse

makeCacheMatrix <- function(x = matrix()) {
        ##Initialize the Inverse of the matrix inverse_x to NULL
        inverse_x <<- NULL
        
        ##function to set a new matrix y
        setmatrix <- function(y){
                ##As a new matrix is created (y), reassigns x to the new matrix y
                ## using <<- to ensure the variable transcends the function environment 
                ##and can be used outside (other functions)
                x <<- y
                ##Asa new matrix y is introduced, we need to blank out the pre-calulated
                ##if we alredy calculated one
                inverse_x <<-NULL
        }
        
        ##function to store (get) the value of a new matrix x
        getmatrix<- function()x 
        
        ##function to set the inverse (not calculate), just get's an argument and passes it
        ##to the variable inverse_x (assigned using<<-)
        
        setinverse <-function(inverse) inverse_x <<- inverse
        
        ##function to get the inverse inverse_x (not calculate)
        getinverse <- function() inverse_x
        
        ##now we need to create the vector (length 4) containing all the functions, which
        ##is ultimately, what the makeCacheMatrix function returns
        list(setmatrix=setmatrix,getmatrix=getmatrix,
             setinverse=setinverse,getinverse=getinverse)
}


## This function calculates or rerieves the inverse of the matrix we pass on the makematrix
##function

cacheSolve <- function(x, ...) {
        #First we pull the mean we already have calculated
        inverse_x <- x$getinverse()
        ##if it's null because we have not calculated it or a new matrix has been
        ##entered
        if(!is.null(inverse_x)){
                message("getting cached inverse")
                ##return the cached inverse
                return(inverse_x)
        }
        ##If no inverse has been cached, let's calculate it
        matrix<-x$getmatrix() ##get matrix we entered
        inverse_x<-solve(matrix) ##calculate the inverse using solve()
        x$setinverse(inverse_x) ##set the inverse
        inverse_x ##return the inverse
        
}
