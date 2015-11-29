makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) { #set vector to new value if needed
                x <<- y      #stores y as x from parent environment
                m <<- NULL   #overwrites mean of previous vector
        }
        get <- function() x  #gets value of current vector
        setmean <- function(mean) {m <<- mean} #stores the mean of the vector in m
        getmean <- function(){m} #outputs the mean of the vector stored in m
        list(set = set, get = get, #store four functions in list, so object that makeVector is assigned to has these functions
             setmean = setmean,
             getmean = getmean)
}
#The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean() #assigns mean value of vector stored in makeVector function to m
        if(!is.null(m)) { #intitiates if statement if m is not NULL, i.e. mean was previously assigned to m
                message("getting cached data") #gets cached data
                return(m) #reads m from cache, which was previously assigned form parent environment
        }
        data <- x$get() #gets current vector
        m <- mean(data, ...) #calculates mean of current vector
        x$setmean(m) #stores the new mean of the vector in m
        m #returns m
}