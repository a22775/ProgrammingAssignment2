# These two functions work together to invert a matrix. The calculation is done just once,
# then the result is stored on cache and used ahead, if the same operation is requested.
# These functions prevent the repeat computation of the same values, speed up the execution of the code
# on long cycles.



# This function generates a list with 4 functions: "set"; "get"; "set_inv"; "get_inv", respectively with the tasks
# storing the input matrix; retrieving the input matrix; storing the inverse matrix; retrieving the inverse matrix. 


makeCacheMatrix <- function(x = matrix()) {

	inv<-NULL

	set <-function(y){
		x<<-y
		inv<<-NULL
	}

	get <-function()x
	
	set_inv <-function(calc_inv){
		inv<<-calc_inv
	}

	get_inv <-function() inv

	list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)

}


# This function computes the inverse of the matrix (with the R funcion solve()) when there is no result stored on the cache,
# and stores it using the previous function.
# If the cache already has a value, then it bypasses the computation and return the result straight from the cache.



cacheSolve <- function(x, ...) {
       

	inv <-x$get_inv()

	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	data <-x$get()
	inv<-solve(data)
	x$set_inv(inv)
	return(inv)
}