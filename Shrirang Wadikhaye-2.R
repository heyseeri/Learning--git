#Name :- Shrirang Wadikhaye
#Student Id :- 131113

#Function to find all pure strategy Nash equilibria in a game
# n_players <- Get the number of players in the game
# n_actions <- Get the number of actions for each player


getAllPureStrategyNE <- function(game) {
  
  n_players <- length(game)
  n_actions <- sapply(game, function(x) dim(x)[1])
  action_profiles <- expand.grid(rep(list(1:max(n_actions)), n_players))
  colnames(action_profiles) <- names(game)
  
  # Define a function to check for Nash equilibrium in a profile
  check_nash <- function(profile) {
    for (player in 1:n_players) {
      # Get the current payoff for the player in the given profile
      current_payoff <- game[[player]][profile[player], ]
      # Get the other actions available to the player
      other_actions <- setdiff(1:n_actions[player], profile[player])
      for (action in other_actions) {
        # Create a new profile with a different action for the player
        new_profile <- profile
        new_profile[player] <- action    
        new_payoff <- game[[player]][new_profile[player], ]  
        cat("Profile:", toString(new_profile), "\n")
        cat("Current Payoff:", current_payoff, "\n")
        cat("New Payoff:", new_payoff, "\n")
        if (any(is.na(new_payoff)) || any(is.na(current_payoff))) {
          stop("Payoff is NA for profile ", toString(new_profile))
        }
        # Check if the new payoff is not greater than or equal to the current payoff
        if (!all(new_payoff >= current_payoff)) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  
  nash_equilibria <- action_profiles[apply(action_profiles, 1, check_nash), , drop=FALSE]
  return(nash_equilibria)
  
}

# Example 1
game <- list(
  "player1" = matrix(c(1, 0, 0, 0), nrow = 2, byrow = TRUE),
  "player2" = matrix(c(1, 0, 0, 0), nrow = 2, byrow = TRUE),
  "player3" = matrix(c(1, 0, 0, 0), nrow = 2, byrow = TRUE)
)
#exmaple 1 
game <- list(
  "player1" = array(c(5, 10, 1, 2), dim = c(2, 2)),
  "player2" = array(c(5, 1, 10, 2), dim = c(2, 2))
)

nash_equilibria <- getAllPureStrategyNE(game)

print(nash_equilibria)

#Example 2 

game <- list(
  "player1" = matrix(c(1, 0, 0, 0, 0, 0, 0, 0), nrow = 2, byrow = TRUE),
  "player2" = matrix(c(1, 0, 0, 0, 0, 0, 0, 0), nrow = 2, byrow = TRUE),
  "player3" = matrix(c(1, 0, 0, 0, 0, 0, 0, 0), nrow = 2, byrow = TRUE),
  "player4" = matrix(c(1, 0, 0, 0, 0, 0, 0, 0), nrow = 2, byrow = TRUE))

nash_equilibria <- getAllPureStrategyNE(game)

print(nash_equilibria)

