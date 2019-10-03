### functions for calculating inverse of a regular matrix,
### already calculated inverse is restored from cache


## setting functions for 
## 1. extracting data from matrix x,
## 2. redefining the content of matrix x and deleting old 
## inverse
## 3. setting new value (invFill) for inverse 
## 4. accessing the inverse
makeCacheMatrix <- function(x=matrix()){
    inv <- NULL
    setData <- function(newData){
        x <<- newData
        inv <<- NULL
    }
    getData <- function() x
    
    setInverse <- function(invToFill) inv <<- invToFill
    getInverse <- function() inv
    return(list(setData = setData,
                getData = getData,
                setInverse = setInverse,
                getInverse = getInverse))
}


## either gets the inverse of x from cache, 
## or calculates it:
## extracts data and sets inverse object for future
cacheSolve <- function(x,...){
    inv <- x$getInverse()
    if (is.null(inv)){
        print("calculating inverse")
        data <- x$getData()
        inv <- solve(data)
        x$setInverse(inv)
    } else print("accessing inverse from cache")
    return(inv)
}
