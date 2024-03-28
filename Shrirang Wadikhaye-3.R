#Name :- Shrirang Wadikhaye  
#Student Id :- 131113

#Function to computing integrals of functions  f a function, e.g., f(x) {x ^ 2};


integrate3d <- function(f, over, n) {
  xlim <- over$x
  ylim <- over$y
  
  # Generate random points within the cuboid and evaluate the function at sampled points
  x_points <- runif(n, min = xlim[1], max = xlim[2])
  y_points <- runif(n, min = ylim[1], max = ylim[2])
  z_points <- f(x_points, y_points)
  
  # Calculate the volume of the cuboid and the Monte Carlo estimate of the integral
  cuboid_volume <- (xlim[2] - xlim[1]) * (ylim[2] - ylim[1])
  integral_estimate <- cuboid_volume * mean(z_points)
  
  return(integral_estimate)
}

#Example 1
f = function(x, y) {cos(x) * y}
over = list(x = c(0, pi / 2), y = c(0, 1))
n = 10^2

result <- integrate3d(f, over, n)

# Display the result
cat("Result of Monte Carlo Integration:", result, "\n")



#Example 2
f = function(x, y) {cos(x) * y}
over = list(x = c(0, pi / 2), y = c(0, 1))
n = 10^5

result <- integrate3d(f, over, n)

# Display the result
cat("Result of Monte Carlo Integration:", result, "\n")
