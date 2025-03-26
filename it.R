it <- function(m){
	ki_=rowSums(m)
	k_j=colSums(m)
	N=sum(m)
	r=nrow(m)
	s=ncol(m)
	my_chi_sq=0
	for(i in 1:r){
		for(j in 1:s){
			my_chi_sq=my_chi_sq+(m[i, j]-ki_[i]*k_j[j]/N)^2/(ki_[i]*k_j[j]/N)
		}
	}

	if(my_chi_sq<=qchisq(p=.05, df=(r-1)*(s-1))){
		return("ACCEPT")
	}
	return("DECLINE")
}
