nd<-function(){
	print("How many samples? (n=)")
	n=scan(what = numeric(), nmax = 1)	
	k=vector()
	c=vector()
	for(i in 1:n){
		k[i] <- as.numeric(readline(prompt = paste(i, ". k value=", sep = "")))
	}
	N=sum(k)
	c_1 <- as.numeric(readline(prompt = "first c value="))
	c_last <- as.numeric(readline(prompt = "last c value="))
	c=seq(from = c_1, to = c_last, length.out = n-1)
	p=calculate_p_values(c)
	expected_values = p * N
	my_chi_sq=sum((k-expected_values)^2/expected_values)
	return(my_chi_sq)
}

calculate_p_values <- function(c){
	p=vector(length = length(c)+1)
	p[1]=pnorm(c[1])
	p[length(p)]=1-pnorm(c[length(c)])
	for(i in 2:(length(c))){
		p[i]=pnorm(c[i])-pnorm(c[i-1])
	}
	return(p)
}

