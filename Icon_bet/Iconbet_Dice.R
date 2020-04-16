
# Iconbet Dice rolling


rm(list = ls())
library(dplyr)


####################### SETTINGS ################################
repeats <- 500 # sampling

# dice rolling range
dice_low <- 3
dice_high <- 97


## logic
chance_win_vector <- data.frame(chance_win = c(1:95)) %>%
  mutate(chance_win_multiplier = 98.5 / chance_win)

pick_chance_win <- dice_high - dice_low + 1
win_multiplier <- chance_win_vector$chance_win_multiplier[(pick_chance_win == chance_win_vector$chance_win)]


counter_list <- list()
icx_wagered_list <- list()

for (i in 1:repeats) {

  ### BET SETTINGS ########
  side_bet_on <- 0 #1: side_one_icx, 2: side_both_icx, 3: side_same_digit
  
  icx_amount <- 500
  icx_bet <- 100
  side_bet <- 1
  network_fee <- 0.004
  
  
  if (side_bet_on != 0) {
    total_icx_bet <- icx_bet + side_bet
  } else {
    total_icx_bet <- icx_bet
  }
  
  counter <- 1
  

repeat {
  
  if (side_bet_on != 0) {
    new_icx_amount <- icx_amount - icx_bet - side_bet - network_fee
  } else {
    new_icx_amount <- icx_amount - icx_bet - network_fee
  }
  
  dice1 <- replicate(1, sample(0:9, 1, replace = TRUE))
  dice2 <- replicate(1, sample(0:9*10, 1, replace = TRUE))
  
  total_dice <- dice1 + dice2
  
  side_one_icx <- ((dice1 == 0) | (dice2 == 0))
  side_both_icx <- ((dice1 == 0) & (dice2 == 0))
  side_same_digit <- dice1 == as.numeric(substr(dice2,1,1))
  
  # print(total_dice)
  
  if (total_dice >= dice_low & total_dice <= dice_high) {
  
  win_outcome <- win_multiplier * icx_bet 
  icx_amount <- new_icx_amount + win_outcome
  
  # print("WIN!!!")
  
  } else {
    icx_amount <- new_icx_amount
    # print("LOST...")
  }
  
  if (side_bet_on == 1) {
    
    if (side_one_icx == TRUE & side_both_icx == FALSE) {
      icx_amount = icx_amount + (side_bet * 5)
    } else {
      icx_amount = icx_amount - side_bet
    }
    
  } else if (side_bet_on == 2) {
    
    if (side_one_icx == TRUE & side_both_icx == TRUE) {
      icx_amount = icx_amount + (side_bet * 95)
    } else {
      icx_amount = icx_amount - side_bet
    }
    
  } else if (side_bet_on == 3) {
    
    if (side_same_digit == TRUE) {
      icx_amount = icx_amount + (side_bet * 9.5)
    } else {
      icx_amount = icx_amount - side_bet
    }
    
}

  
  if (side_bet_on != 0) {
    total_icx_bet = total_icx_bet + icx_bet + side_bet
  } else {
    total_icx_bet = total_icx_bet + icx_bet
  }
  
  
  print(icx_amount)
  counter = counter + 1
  
  
  
  if (icx_amount < icx_bet) {
    print(paste("Rolled", counter, "times"))
    print(paste("Total ICX wagered is", total_icx_bet))
    break
  }
  
} ## no loop (repeat only)

counter_list[i] <- counter
icx_wagered_list[i] <- total_icx_bet


# boxplot(unlist(counter_list))

} ## loop


counter_unlist <- unlist(counter_list) 
icx_wagered_list <- unlist(icx_wagered_list)

summary(counter_unlist)
summary(icx_wagered_list)

plot(icx_wagered_list)
# boxplot(counter_unlist)


# side:
# 19% -> 5x
# 1% >- 95x
# 10% -> 9.5
  
  