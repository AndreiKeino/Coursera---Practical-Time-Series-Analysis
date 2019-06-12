# Q 5

1 + 2 * 0.5^2

# Q 6


(1 * 0.5)/(1 + 2 * 0.5^2)

# Q 7

set.seed=1
(ac <- acf(arima.sim(n=1000, model=list(ma=c(0.5, 0.5)))))
ac$acf[2]