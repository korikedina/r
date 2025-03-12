p <- function(a, x, y){
	pv<-vector()
	for(i in 1:length(x)){
		pv[i]=prod((a-(x[-i]))/(x[i]-(x[-i])))
	}
	print(pv)
	return(sum(y*pv))
}