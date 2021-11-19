##Week3_Assignment
##First function creation
makecachematrix <-function( a= matrix())
{
  ##setting variable to null
  inv  <- NULL
  set <-function(b)
    {
      a <<-b
      inv <<-NULL
    }
      get <-function() {a}
      ##set value of inverse
      setinverse <-function(inverse) {inv <<- inverse}
      
      ##Getting value of inverse
      getinverse <-function()        {inv}
      ##create list
      list( set =set, get =get, setinverse= setinverse, getinverse=getinverse)
}
## End of make cache matrix

##Second function cachesolve creation
cachesolve <-function(a,...)
{
  inv <-a$getinverse ()
  ## check if already cached
  if (!is.null (inv))
       {
  ##Display message  
          message("Retreiving cached data")
          return (inv)
       }
  matr <- a$get()
  inv <-solve(matr, ...)
  a$setinverse(inv)
  inv
}
## End of second function 

##Testing
showmatrix <-makecachematrix(matrix (1:4, nrow=2, ncol=2))
showmatrix$get()
cachesolve(showmatrix)
