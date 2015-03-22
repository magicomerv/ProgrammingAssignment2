## The following functions will help to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL #to assign NULL to the object to contain x's inverse
      set <- function(y){ #set assigns a new matrix to x and sets the inverse to be null
            x <<- y
            inverse <<- NULL
      }
      
      get <- function() x #get returns the matrix

      setinverse <- function(inv) inverse <<- inv #setinverse sets a new inverse
      getinverse <- function() inverse #getinverse returns the inverse 
      
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)   #returns a list of the functions
}

## cacheSolve calculates the inverse of a matrix, provided that there are no inverse matrix already cached.
## cacheSolve returns the cached inverse if already cached.


cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) { #checks if cached data exists.
            message("getting cached data")
            return(inverse)   #returns inverse matrix
      }
      
      data <- x$get() #takes matrix from object to be assigned into data
      inverse <- solve(data, ...) #calculates the inverse of the data matrix and assigns into inverse
      x$setinverse(inverse) #sets the inverse matrix into the object
      
}

