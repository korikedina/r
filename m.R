modus <- function(x){
	x<-table(x)
	return(as.numeric(names(x)[which.max(x)]))
}

median <- function(x){
	n=length(x)
	x<-sort(x)
	if(n%%2==1){
		return(x[n/2+1])
	}
	return(c(x[n/2], x[n/2+1]))
}

m <- function(x){
	return(sum(x)/length(x))
}