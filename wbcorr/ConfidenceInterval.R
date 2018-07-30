function (r, critical_value, N) {
         z <- 0.5 * (log(1 + r) - log(1 - r))
         limit <- z + critical_value * sqrt(1 / (N - 3))
         tanh(limit)
}