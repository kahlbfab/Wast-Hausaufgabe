

?qpois
ppois(q = 3, 8.4)
qpois(p= 0.975, 8.4)
ppois(q = 14, 8.4)

library(lattice)
t<-seq(-2*pi, 2*pi, length.out=200)
cloud(z~x+y,data.frame(x=3*cos(t),y=3*sin(t), z=2*t))

t <- seq(0,10*pi, 0.1)
x <- t*sin(t)
y <- t*cos(t)

t <- seq(0,10*pi, 0.1)

data.frame(x,y)
plot(x~y, type = "l")
dev.off()

ppois(6, lambd.8)
qpois(p=0.719, lambda = 9.8)
?poisson.test
11/8.4
11-8.4
ppois(c(15,16), lambda = 9.8)
poisson.test(11, r=9.8, alternative = "two.sided")



pnorm(192.8433, 200, 20/sqrt(30))
pnorm(207.1567, 200, 20/sqrt(30))

2*pnorm(193.1, 200, 20/sqrt(30))

library(BSDA)
z.test(193.1, 200, 20/sqrt(30))
z.test(rnorm(30, mean = 200, sd = 20/sqrt(30)), sigma.x =20**2 , alternative = "two.sided")
t.test(rnorm(30, mean = 200, sd = 20/sqrt(30)), mu = 193.1, alternative = "two.sided")
plot(rnorm(100, 200, 20/sqrt(30)), type ="l")
x <- seq(190, 210, 0.1)

plot(x = x, y = dnorm(x, 200, 20/sqrt(30)),type = "l")
?plot()
?pt
z.test(rnorm(30, mean= 0, sd = 1), sigma.x = 193.1, alternative = "two.sided")

plot(qnorm(0.95))
qnorm(0.05)
qnorm(0.95+(0.05/2))

x <- seq(-8, 8, 0.1)
plot(x = x, y = dt(x, df= 1), type = "l")
qt(0.975, 1)

