# ----------------- Deck Functions -----------------

define_deck <- function() {
  suits <- c(1:4)  # 1 = Hearts, 2 = Diamonds, 3 = Clubs, 4 = Spades
  ranks <- c(2:14) # 11=Jack, 12=Queen, 13=King, 14=Ace
  
  deck <- expand.grid(Rank = ranks, Suit = suits, stringsAsFactors = FALSE)
  deck$Color <- ifelse(deck$Suit %in% c(1, 2), "Red", "Black")
  
  return(deck)
}

reshuffle_check <- function(deck) {
  if (nrow(deck) == 0) {
    deck <- define_deck()
  }
  return(deck)
}

# ----------------- Simulation Function -----------------

simulation <- function(n) {
  drinks_vec <- numeric(n)  # Store number of drinks per game
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  i <- 1
  while (i <= n) {
    setTxtProgressBar(pb, i)
    drinks <- 0
    stage <- 1
    deck <- define_deck()
    
    while (stage <= 4) {
      deck <- reshuffle_check(deck)
      drawn_card <- deck[sample(nrow(deck), 1), ]
      deck <- deck[!rownames(deck) %in% rownames(drawn_card), ]
      
      # Stage 1 — Color Guess
      if (stage == 1) {
        color_choice <- sample(c("Red", "Black"), 1)
        if (drawn_card$Color != color_choice) {
          drinks <- drinks + 1
          stage <- 1  # Restart game
          next
        }
        first_card <- drawn_card
        stage <- 2
      }
      
      # Stage 2 — High/Low Guess
      else if (stage == 2) {
        second_card <- drawn_card
        if (first_card$Rank %in% 2:7) {  # Guess Up
          if (second_card$Rank <= first_card$Rank) {
            drinks <- drinks + 1
            stage <- 1
            next
          }
        } else {  # Guess Down
          if (second_card$Rank >= first_card$Rank) {
            drinks <- drinks + 1
            stage <- 1
            next
          }
        }
        stage <- 3
      }
      
      # Stage 3 — Inside/Outside Guess
      else if (stage == 3) {
        third_card <- drawn_card
        diff <- abs(first_card$Rank - second_card$Rank)
        
        if (diff >= 6) {  # Inside
          lower <- min(first_card$Rank, second_card$Rank)
          upper <- max(first_card$Rank, second_card$Rank)
          correct <- seq(lower + 1, upper - 1)
          
          if (!(third_card$Rank %in% correct)) {
            drinks <- drinks + 1
            stage <- 1
            next
          }
        } else {  # Outside
          if (first_card$Rank == second_card$Rank) {
            if (third_card$Rank == first_card$Rank) {
              drinks <- drinks + 1
              stage <- 1
              next
            }
          } else {
            lower <- min(first_card$Rank, second_card$Rank)
            upper <- max(first_card$Rank, second_card$Rank)
            correct <- c(seq(2, lower - 1), seq(upper + 1, 14))
            
            if (!(third_card$Rank %in% correct)) {
              drinks <- drinks + 1
              stage <- 1
              next
            }
          }
        }
        stage <- 4
      }
      
      # Stage 4 — Suit Guess
      else if (stage == 4) {
        suits_remaining <- c(13, 13, 13, 13) # Hearts, Diamonds, Clubs, Spades
        
        deck <- reshuffle_check(deck)
        fourth_card <- deck[sample(nrow(deck), 1), ]
        deck <- deck[!rownames(deck) %in% rownames(fourth_card), ]
        
        suits_remaining[first_card$Suit] <- suits_remaining[first_card$Suit] - 1
        suits_remaining[second_card$Suit] <- suits_remaining[second_card$Suit] - 1
        suits_remaining[third_card$Suit] <- suits_remaining[third_card$Suit] - 1
        
        suit_choice <- which.max(suits_remaining)
        
        if (suit_choice != fourth_card$Suit) {
          drinks <- drinks + 1
          stage <- 1
          next
        }
        stage <- 5
      }
    }
    
    drinks_vec[i] <- drinks
    i <- i + 1
  }
  close(pb)
  return(drinks_vec)
}

n_games <- 100000
results <- simulation(n_games)

# Histogram of drinks per game
hist(results,
     breaks = 20,
     col = "skyblue",
     main = "Drinks to Complete a Game",
     xlab = "Number of Drinks")

# Basic stats
mean_drinks <- mean(results)
sd_drinks <- sd(results)

cat("Average drinks to complete:", mean_drinks, "\n")
cat("Standard deviation:", sd_drinks, "\n")

# 95% Confidence Interval
se <- sd_drinks / sqrt(n_games)
ci_lower <- mean_drinks - 1.96 * se
ci_upper <- mean_drinks + 1.96 * se

cat("95% CI:", ci_lower, "-", ci_upper, "\n")

