set.seed(123)

fx<- function(x){
    -x^2 + (x) + 6
}

gx<- function(x){
    -x^2 + (6*x) -5 
}

xmin <- -2
xmax <- 5

ymin <- 0
ymax <- 6.25

nsamples<- 1000

x <- runif(nsamples, min= xmin, max= xmax)
y <- runif(nsamples, min= ymin, max= ymax)


dentro_f <-  (fx(x) >= 0)  & (y<= fx(x)) 
dentro_g <- (gx(x) >= 0)  & (y<= gx(x)) 

result <- dentro_f | dentro_g

cat("area estimada",sum(result))