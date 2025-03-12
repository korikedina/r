ttest1 <- function(x, m, fd){
	t=(mean(x)-m)/(Sn_star(x)^2/sqrt(length(x)))
	print(t)
	tn=qt(fd, length(x))
	if(t>=-tn&&t<=tn){
		print("accept")
	}else{
		print("decline")
	}
}

Sn_star <- function(x){
	n=length(x)
	return(sqrt(1/(n-1)*sum((x-mean(x))^2)))
}

ttest2 <- function(x, y, fd){
	n=length(x)
	m=length(y)
	t=((mean(x)-mean(y))/sqrt(Sn_star(x)^2*(n-1)+Sn_star(y)^2*(m-1)))*sqrt((n*m*(n+m-2))/(n+m))
	print(t)
	tn=qt(fd, length(x))
	if(t>=-tn&&t<=tn){
		print("accept")
	}else{
		print("decline")
	}
}