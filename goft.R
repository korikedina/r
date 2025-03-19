chi_sq_test <- function(k, p){
	if(sum(p)!=1){
		return("Incorrect p values!")
	}
	if(length(k)!=length(p)){
		return("Samples are not the same length!")
	}
	if(any(k<0)){
		return("Negative k value!")
	}
	if(any(p<0)){
		return("Negative p value!")
	}
	N=sum(k)
	chi_sq=sum((k-N*p)^2/(N*p))
	print(chi_sq)
	if(chi_sq>=0 && chi_sq<=qchisq(.05, length(p)-1)){
		return("accept")
	}else{
		return("decline")
	}
}