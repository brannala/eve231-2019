# EVE 131 R in a nutshell
# some basic math functions in R

# addition, subtraction and multiplication

a=4+3+6.789
a
b=124*(32+78*19)
b
c=a+b*42
c

# powers and roots
x=4.3
z=x^2
z
sqrt(z)
y=x^12
y
y^(1/12)

# other common functions
log(x)
help(log)
exp(log(x))
factorial(10)

# vectors
a1 = c(12.3,4,6.6,10.9,23)
a1
length(a1)
a1[2]
a1[1]^2
a1^2
a2 <- a1[1:4]
a2
a3 <- a2[-2]
a3

# functions are vectorized
sum(a1)
prod(a1)
sqrt(a1)

# simple statistics
mean(a1)
var(a1)
sd(a1)
sqrt(var(a1))
b1 = c(11,4,7,9,19)
cor(a1,b1)
cov(a1,b1)/sqrt(var(a1)*var(b1))

# find critical value for chi square test statistic using quantile function
qchisq(0.95,1)

# find tail probability for chi square test statistic using cumulative distribution function
1-pchisq(3.84,1)

# create your own function definition
myfactorial = function(x)
{
	fa=1;
	while(x>1)
		{
		fa = fa*x;
		x=x-1;
		}
	fa
}

myfactorial(10)
factorial(10)

# installing and using the pedigree package

install.packages("devtools",dependencies=TRUE)
library("devtools")
install_github('brannala/pedigree')

getwd()
