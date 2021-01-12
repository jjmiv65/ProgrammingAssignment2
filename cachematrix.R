## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#stores a matrix and inverse, also allows use of set function to put in a new matrix
makeCachematrix <-function (x=matrix ()){  #initialize x as function argument to define default value as empty vector
  inv<-NULL  #initialize inv as object in makeCachematrix parent environment
  set <- function (y) { #defines the set function of makeCachematrix
    x<<-y  # (superassignment operator) assigns value on the right side of the operator to an object in the parent environment named by the object on the left side of the operator
    inv<<- NULL #assign the value of NULL to the inv object in the parent environment and clears any value of inv that had been cached by a prior execution of cacheSolve
  }
  get <-function() x #defines getter for x; since x is not defined within get(), R retrieves from the parent environment of makeVector()
  setinverse<-function(inverse) inv<<-inverse #defines setter for inverse inv; assign the input argument to the value of inv in the parent environment. i.e. lexical scoping 
  getinverse<-function() inv #define the getter for the inverse inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #assigns each of these functions as an element within a list(), and returns it to the parent environment
}       # allows use the $ form of the extract operator to access the functions by name 
# Solve for Inverse Matrix, store value, return message if in cache
cacheSolve<-function(x,...) { #set cacheSolve as an anonymous function
  inv<-x$getinverse() #retrieves getinverse from list previously defined and assigns to inv
  if (!is.null(inv)){ #check for cache data, if not Null print message and inv, otherwise solve function
    message("getting cached data")
    return(inv)
  }
  mat<-x$get() #retrieves get from list previously defined and assigns to mat
  inv<-solve(mat,...) #solves inverse matrix
  x$setinverse(inv) #reset 
  inv #returns solved inverted matrix
}

