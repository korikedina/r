ztest1 <- function(x, m){
	n=length(x)
	u=(m-mean(x))/(sd(x)/sqrt(n))
	print(u)
	if(u>=-1.96&&u<=1.96){
		print("accept")
	}else{
		print("decline")
	}
}

ztest2 <- function(x, y, m){
	n=length(x)
	m=length(y)
	u=(mean(x)-mean(y))/(sd(x)^2/n + sd(y)^2/m)
	print(u)
	if(u>=-1.96&&u<=1.96){
		print("accept")
	}else{
		print("decline")
	}
}