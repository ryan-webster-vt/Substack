
# Define Deck -------------------------------------------------------------

suits <- c(1:4) # Hearts, Diamonds, Clubs, Spades
ranks <- c(2:14) # 11 = Jack, 12 = Queen, 13 = King, 14 = Ace
colors <- c("Red", "Red", "Black", "Black")

deck <- expand.grid(Rank = ranks, Suit = suits, stringsAsFactors = FALSE)

deck$Color <- ifelse(deck$Suit %in% c(1, 2), "Red", "Black")


# Game Simulation Function ------------------------------------------------

simulation <- function(n, deck) {
  wins <- 0
  win_history <- numeric(n)
  
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  failure <- numeric(4)
  
  
  for (i in 1 : n) {
    setTxtProgressBar(pb, i)
    
    cards <- deck[sample(nrow(deck), 4), ]
    first_card <- cards[1, ]
    second_card <- cards[2, ]
    third_card <- cards[3, ]
    fourth_card <- cards[4, ]
    
    # First Round
    color_choice <- sample(c("Red", "Black"), 1)
    
    if (first_card$Color != color_choice) {
      failure[1] <- failure[1] + 1
      next
    }
    
    # Second Round
    high_or_lower <- sample(c(TRUE, FALSE), 1)
    
    if (high_or_lower) { # Choose higher
      if (first_card$Rank >= second_card$Rank) {
        failure[2] <- failure[2] + 1
        next
      }
    } else { # Choose lower
      if (first_card$Rank <= second_card$Rank) {
        failure[2] <- failure[2] + 1
        next
      }
    }
    
    # Third Round
    inside_or_outside <- sample(c(TRUE, FALSE), 1)
  
    if (inside_or_outside) { # Choose Inside
      if (third_card$Rank <= min(first_card$Rank, second_card$Rank) |
          third_card$Rank >= max(first_card$Rank, second_card$Rank)) {
        failure[3] <- failure[3] + 1
        next
      }
    } else {
      if (third_card$Rank >= min(first_card$Rank, second_card$Rank) &
          third_card$Rank <= max(first_card$Rank, second_card$Rank)) {
        failure[3] <- failure[3] + 1
        next
      }
    }
    
    
    # Fourth Round
    suits_choice <- sample(c(1:4), 1)
    
    if (suits_choice != fourth_card$Suit) {
      failure[4] <- failure[4] + 1
      next
    }
    
    wins <- wins + 1
    win_history[i] <- 1
  }
  close(pb)
  
  win_rate <- cumsum(win_history) / seq_len(n)
  smoothed_rate <- zoo::rollmean(win_rate, k = 100, fill = NA)
  
  plot(smoothed_rate,
       type = "l", col = "blue", lwd = 1, lty = 1,
       xlab = "Number of Simulations",
       ylab = "Smoothed Win Rate",
       ylim = c(0, 0.1),
       main = "Smoothed Win Rate Over Time")
  abline(h = mean(win_rate), col = "red", lty = 3)
  
  return(list(wins, smoothed_rate, failure))
}

n = 100000
number_of_sims <- 10
prop_list <- numeric(number_of_sims)
smoothed_rate_list <- matrix(NA, ncol = n, nrow = number_of_sims)
failure_list <- matrix(NA, ncol = 4, nrow = number_of_sims)


for (i in 1:number_of_sims) {
  cat("Running simulation", i, "of", number_of_sims, "\n")
  output <- simulation(n, deck)
  total_wins <- output[[1]]
  smoothed_rate_list[i, ] <- output[[2]]
  prop_list[i] <- total_wins / n
  failure_list[i, ] <- output[[3]]
}

summary(failure_list)


# Visualize ---------------------------------------------------------------

matplot(t(smoothed_rate_list), type = "l", lty = 1, lwd = 1,
        col = rainbow(number_of_sims),
        xlab = "Number of Simulations",
        ylab = "Smoothed Win Rate",
        main = "Smoothed Win Rate Across Simulations",
        ylim = c(0, 0.1))
abline(h = mean(prop_list), col = "red", lty = 2)

hist(prop_list, 
     freq = FALSE, 
     col = "skyblue", 
     border = "white",
     main = "Distribution of Win Proportions",
     xlab = "Win Proportion", 
     ylab = "Density")

# Add a density curve
lines(density(prop_list), 
      col = "darkblue", 
      lwd = 2)

# Add a vertical line at the mean
abline(v = mean(prop_list), 
       col = "red", 
       lwd = 2, 
       lty = 2)


qqnorm(prop_list)
qqline(prop_list, col = "red")

mean_prop <- mean(prop_list)
se_prop <- sd(prop_list) / sqrt(length(prop_list))

ci_lower <- mean_prop - 1.96 * se_prop
ci_upper <- mean_prop + 1.96 * se_prop

c(ci_lower, ci_upper)
