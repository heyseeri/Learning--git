#Name :- Shrirang Wadikhaye  
# Student Id :- 131113



### Plotting
plot(as.raster(d0))



pathQ <- function(d0, startPoint, endRegion) {
  queue <- list(startPoint)  # Initialize the queue with the source coordinates
  
  visited <- matrix(FALSE, nrow(d0), ncol(d0))
  visited[startPoint[1], startPoint[2]] <- TRUE  # Mark the source node as visited
  
  while (length(queue) > 0) {
    current <- queue[[1]]  # Dequeue the first element from the queue
    queue <- queue[-1]  # Remove the first element from the queue
    
    if (current[1] >= endRegion[1] && current[1] <= endRegion[2] &&
        current[2] >= endRegion[3] && current[2] <= endRegion[4]) {
      return(TRUE)  # Destination found
    }
    
    # Visit all adjacent nodes
    adjacents <- list(c(current[1] - 1, current[2]), c(current[1] + 1, current[2]), c(current[1], current[2] - 1), c(current[1], current[2] + 1))
    for (adj in adjacents) {
      if (adj[1] >= 1 && adj[1] <= nrow(d0) && adj[2] >= 1 && adj[2] <= ncol(d0) && d0[adj[1], adj[2]] && !visited[adj[1], adj[2]]) {
        queue <- c(queue, list(adj))  # Enqueue the adjacent node
        visited[adj[1], adj[2]] <- TRUE  # Mark the adjacent node as visited
      }
    }
  }
  
  return(FALSE)  # Destination not found
}

#Example  1
startPoint <- c(1, 1)  # x, y
endRegion <- c(387, 413, 322, 348)  # x1, x2, y1, y2

# Testing if there is a path leading from the starting point to the end region
pathExists <- pathQ(d0=d0, startPoint, endRegion)
print(pathExists)  # This will print TRUE if a path exists, otherwise FALSE




#Example 2
### End region
endPosition <- list(x = 220:230, y = 325:335)

### Starting point
startPoint <- list(x = 1, y = 1)

#Example  1
startPoint <- c(1, 1)  # x, y
endRegion <- c(220, 230, 325, 335)  # x1, x2, y1, y2

# Testing if there is a path leading from the starting point to the end region
pathExists <- pathQ(d0=d0, startPoint, endRegion)
print(pathExists)  # This will print TRUE if a path exists, otherwise FALSE

