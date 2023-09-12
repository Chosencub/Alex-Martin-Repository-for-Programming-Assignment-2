makeCacheMatrix <- function(x = numeric()) {                #sets the function and denotes base variable as a numeric vector
        m <- NULL                                           #sets variable to fill later
        set <- function(y) {                                #makes a set function that will write on the variable x with new variable y
                x <<- y
                m <<- NULL
        }
        get <- function() x                                 #make a get function of the x variable
        setInverse <- function(solve) m <<- solve           #make a setInverse variable that will set the answer to the "solve" function in the m variable
        getInverse <- function() m                          #make a getInverse variable that will pull the result of the m variable
        list(set = set, get = get,                          #puts all these variables in a list for later use
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {                            #sets the function and recalls x as our input variable as well as anything else pulled with x using "..."
        m <- x$getInverse()                                 #pulls the data list in x of the getInverse line
        if(!is.null(m)) {                                   #asks to see if data is not NULL, if TRUE (there by not being NULL) will go get data saved under m variable
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                     #sets data from the x variable get list item as new variable data
        m <- solve(data, ...)                               #uses the solve function on the data variable to create the inverse
        x$setInverse(m)                                     #retreives m variable from x data set list item setInverse
        m                                                   #keeps m in the global environment
}
