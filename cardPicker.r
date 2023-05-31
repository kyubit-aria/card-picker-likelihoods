deck <- c(1:52) # The deck with N cards

# Function Sample N cards without replacement
#  Returns a vector of N cards sampled from the deck
sampleNWithoutReplacement <- function(noSamples) {
    sample(deck, noSamples, replace = FALSE)
}

# check if player 1 has won out of N players
isVictorious <- function(sample) {
    for (playerCard in sample[-1]) if (sample[1] < playerCard) return(0)
    return(1)
}

# Function to calculate the likelihood of victory
likelihoodOfVictory <- function(samples, players = 4) {
    # Do N experiments of N samples
    samples <- replicate(samples, sampleNWithoutReplacement(players), simplify = FALSE)

    # Check if player 1 has won for each sample
    result <- unlist(lapply(samples, isVictorious))

    # Calculate the likelihood of victory and display the results
    victories <- sum(result)
    return(victories / length(result))
}

runExperiementNTimes <- function(epochs,samples,players) {
    results <- c()
    for(i in 1:epochs) {
        results <- c(results, likelihoodOfVictory(samples,players))
    }
    plot(
        (1:epochs), results,
        type = "o",
        main = "Likelihoods of Player1 Winning",
        xlab = "Epoch",
        ylab = "Probability of Player1 Winning")

    mean_value <- mean(results)
    variance_value <- var(results)
    abline(h = mean_value, col = "red", lwd = 2)
    mtext(paste("Mean =", mean_value), side = 3, line = -2, col = "red")
    mtext(paste("Variance =", variance_value), side = 3, line = -1, col = "blue")
    if (interactive()) {
        Sys.sleep(10)  # Add a delay to view the plot for 10 seconds
    }
}

runExperiementNTimes(150,10000,4)