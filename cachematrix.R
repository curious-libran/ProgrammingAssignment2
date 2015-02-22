## There are two functions makeCacheMatrix() & CacheSolve()
## "makeCacheMatrix()" 
##  It has 1 formal argument "x" which is of class type "matrix()" i.e input to the function.  
## It also has a variable "m" i.e the cache object

## Purpose of m - is to store the value of matrix inverse once calculated. Rerunning the script on same matrix 
## will be faster next time because, it'll simply retrieve the inverse from "m" thus, saving on computation time

## "makeCacheMatrix" has 4 functions nested within it 
##  set(),get() used to set & get matrix and setmatrix() & getmatrix() to set & get inverse of matrix.
## 1.) set()- it uses a super assignment operator to set  "x" to a new user inputted
## matrix "y". It also sets the cache object to NULL to reset it.
## 2.) get() - gets the user inputted  matrix stored in variable x
## 3.) setmatrix() - sets cache object "m" to a matrix inverse value calculated in its parent enviornment 
##  it uses super assignment  "<<-" operator to achieve this
## 4.) getmatrix() - gets the value of m i.e inverse of matrix

## "cacheSolve()" function computes the inverse of the matrix i.e created using makeCacheMatrix function 
## it first checks if cache object "m" has inverse of matrix "x" by making a call to getmatrix function
##  if getmatrix returns a non NULL value then it simply returns the cached inverse of matrix
## else it invokes the get() function from mackeCacheMatrix to get user input matrix x.Stores this matrix in 
## new variable data, computes the inverse of the inputted matrix  and stores it in the cache object
## m. Invokes setmatrix function to set the cache object "m" to the inverse of matrix
## returns m in the end. 

## Henceforth,next time cacheSolve is called to compute inverse of the same matrix again it'll first look 
## for the inverse of the matrix in its cache object and returns the cached object if it has already been calculated
## before thus saving on computation time.

## Description makecacheMatrix function : It takes matrix as an input and primarily creates a cache
## object m which stores the inverse of input matrix.

makeCacheMatrix <- function(x=matrix()) {
        m<-NULL             # m assigned to NULL in parent function
        set <- function(y) {
                x <<- y     # uses super assignment operator to set "x" in parent function to new value "y"  
                m<<- NULL   # resets m in parent function to NULL 
        }
        get <- function() x # gets the user inputted matrix
        setmatrix <- function(solve) m <<- solve   # sets the inverse of matrix in cache object "m"
        getmatrix <- function() m # gets the inverse of matrix from cache object "m"
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Description cacheSolve function : it takes input of type matrix and checks the cache object
## m to see if the inverse of matrix already exists else computes the inverse and stores it in
##  m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()         
        if(!is.null(m)) {        # checks if m is not NULL then returns the cached inverse stored in m
                message("getting cached data")
                return(m)
        }
        data <- x$get()          # assigns the inputted matrix to data
        m <- solve(data, ...)     # computes inverse of data and assigns it to cache object "m"
        x$setmatrix(m)             # sets the inverse of  matrix "x" to "m" 
        m                        # returns m to the caller
}