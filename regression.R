det <- function(matrix){
	print(matrix)
	d=0
	multiplier=1
	if(nrow(matrix)!=2){
		for (i in (1:ncol(matrix))){
			substraction=matrix[-1,-i]
			d=d+multiplier*matrix[1,i]*det(substraction)
			multiplier=(-1)*multiplier
		}
	}else{
		return((matrix[1,1]*matrix[2,2])-(matrix[2,1]*matrix[1,2]))
	}
	print(d)
	return(d)
}

coefs <- function(x, y, m){
	coefs=matrix(nrow=m+1, ncol=m+1)
	for(i in 1:m){
		for(j in 1:(m+1)){
			coefs[j,i]=sum(x^(2*m+1-j))
		}
	}
	for(i in 1:(m+1)){
		coefs[i, m+1]=sum(y*x^(m+1-i))
	}
	return(coefs)
}

reg <- function(x, y, m) {
	a = numeric(m+1)  
	cfs = coefs(x, y, m)

  
	for (i in 1:(m+1)) {
		cfsm = cfs
		cfsm[, i] = cfs[, m+1] 
		a[i] = det(cfsm) / det(cfs) 
	}
  
  return(a)
}

