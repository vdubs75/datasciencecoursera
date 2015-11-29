## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { #set matrix to new value if needed
                x <<- y      #stores y as x from parent environment
                inv <<- NULL   #overwrites inverse of previous matrix
        }
        get <- function() x  #gets value of current matrix
        setsolve <- function(solve) {inv <<- solve} #stores the mean of the vector in m
        getsolve <- function(){inv} #outputs the mean of the vector stored in m
        list(set = set, get = get, #store four functions in list, so object that makeVector is assigned to has these functions
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        inv <- x$getsolve() #assigns inverse of matrix stored in makeCacheMatrix function to inv
        if(!is.null(inv)) { #intiates if statement if inv is not NULL, i.e. solution of solve was previously assigned to inv
                message("getting cached data") #gets cached data
                return(inv) #reads inv from cache, which was previously assigned form parent environment
        }
        data <- x$get() #gets current matrix
        inv <- solve(data, ...) #calculates inverse of current matrix
        x$setsolve(inv) #stores the new inverse of the matrix in inv
        inv #returns inv
}