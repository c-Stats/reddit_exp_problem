sim_rv <- function(r1, r2, t){

	T1 <- rexp(1, r1)
	if(T1 < t){T1} else {t + rexp(1, r2)}

}


cdf_func <- function(x, r1, r2, t){

	if(x < t){1 - exp(-r1 * x)} else {1 - exp(-r1 * t) + exp(-r1 * t) * (1 - exp(-r2 * (x - t)))}


}


set.seed(420)

r1 = 2
r2 = 3
t = 1

s <- function(){sim_rv(r1, r2, t)}
F <- function(x){cdf_func(x, r1, r2, t)}

values <- sort(replicate(10^5, s()))

empirical_cdf <- ecdf(values)

x_axis <- sort(values)
y_axis_cdf <- sapply(x_axis, F)
y_axis_ecdf <- sapply(x_axis, empirical_cdf)

plot(x_axis, y_axis_cdf, type="l",col="red")
lines(x_axis ,y_axis_ecdf, col="green")
