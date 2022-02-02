library(httr)
library(jsonlite)

options(warn=-1)
euroRoulette <- function(budget, numSpins, number, bet) {
  startedBudget <- budget
  wins <- 0
  for (i in 1:numSpins) {
    if (budget <= 0) break

    outcome <- sample(0:36,1)
    if (outcome == number) {
      budget <- budget + 35 * bet
      wins <- wins + 1
    } else {
      budget <- budget - bet
    }
  }
  cat(sprintf("Oczekiwany wynik wylosowania w ruletce: %i , to: %.10f %% \n", number, (100*wins/numSpins)))
  cat(sprintf("wygrana wynosi %i \n", budget - startedBudget))
  cat(sprintf('liczba wygranych: %i \n\n', wins))
}
res <- GET("http://www.randomnumberapi.com/api/v1.0/random", query = list(min = 0, max = 36, count = 30))
data <- fromJSON(rawToChar(res$content))
print(data)
print(sample(0:36,30))
euroRoulette(1000000,1000000, sample(0:36,1), 1)
#dbinom(0,30,1/37)

lotto <- function(numSpins, numbers) {
  wins <- 0
  for (i in 1:numSpins) {
    counter <- 0
    outcome <- sample(1:49, 6)

    for (i in 1:6) {
      if (!is.na(match(numbers[i], outcome))) {
        counter <- counter + 1
      }
    }

    if (counter == 6) {
      wins <- wins + 1
    }
    #cat(sprintf('Wygrane liczby: '))
    #for (i in outcome) {
    #  cat(sprintf('%i ', i))
    #}
    #cat(sprintf(' | -> '))
    #for (i in numbers) {
    #  cat(sprintf('%i ', i))
    #}
    #cat(sprintf('\n '))
  }

  cat(sprintf('Losowane liczby: '))
  for (i in numbers) {
    cat(sprintf('%i ', i))
  }
  cat(sprintf('\n Oczekiwany wynik wylosowania w lotto, to: %f %% \n', 100*wins/numSpins))
  cat(sprintf('liczba wygranych: %i \n\n', wins))
}

lotto(10000000, sample(1:49, 6))

multiMulti <- function(budget, numSpins, numbers, plus, ticketCost) {
  wins <- 0.0
  for (i in 1:numSpins) {
    if (budget <= 0) break
    ifelse (plus, budget <- budget - ticketCost, budget <- budget - ticketCost * 2)
    counter <- 0
    outcome <- sample(1:80, 20)

    for (i in 1:numbers) {
      if (!is.na(match(numbers[i], outcome))) {
        counter <- counter + 1
      }
    }
    if (counter == length(numbers)) {
      wins <- wins + 1
    }

  }
  cat(sprintf('Losowane liczby: '))
  for (i in numbers) {
    cat(sprintf('%i ', i))
  }
  cat(sprintf('\n Oczekiwany wynik wylosowania w Multi Multi, to: %f %% \n', 100*wins/numSpins))
  cat(sprintf('liczba wygranych: %i \n\n', wins))
}
multiMulti(20000000, 10000000, sample(1:80, 10), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 9), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 8), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 7), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 6), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 5), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 4), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 3), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 2), FALSE, 2)
multiMulti(20000000, 10000000, sample(1:80, 1), FALSE, 2)

euroJackpot <- function (numSpins, numbers5, numbers2) {
  wins <- 0
  for (i in 1:numSpins) {
    counter <- 0
    outcome <- sample(1:50, 5)
    outcome2 <- sample(1:10, 2)
    for (i in 1:5) {
      if (!is.na(match(numbers5[i], outcome))) {
        counter <- counter + 1
      }
    }
    for (i in 1:2) {
      if (!is.na(match(numbers2[i], outcome2))) {
        counter <- counter + 1
      }
    }
    if (counter == 7) {
      wins <- wins + 1
    }
  }
  cat(sprintf('Losowane liczby: '))
  for (i in numbers5) {
    cat(sprintf('%i ', i))
  }
  cat(sprintf(' i '))
  for (i in numbers2) {
    cat(sprintf('%i ', i))
  }
  cat(sprintf('\n Oczekiwany wynik wylosowania w EuroJackpot, to: %f %% \n', 100*wins/numSpins))
  cat(sprintf('liczba wygranych: %i \n\n', wins))
}

euroJackpot(10000000, sample(1:50, 5), sample(1:10, 2))

powerball <- function (numSpins, numbers5, numbers) {
  wins <- 0
  for (i in 1:numSpins) {
    counter <- 0
    outcome <- sample(1:69, 5)
    outcome2 <- sample(1:26, 1)
    for (i in 1:5) {
      if (!is.na(match(numbers5[i], outcome))) {
        counter <- counter + 1
      }
    }
    if (numbers == outcome2) {
      counter <- counter + 1
    }
    if (counter == 6) {
      wins <- wins + 1
    }
  }
  cat(sprintf('Losowane liczby: '))
  for (i in numbers5) {
    cat(sprintf('%i ', i))
  }
  cat(sprintf(' i '))
  for (i in numbers) {
    cat(sprintf('%i ', i))
  }
  cat(sprintf('\n Oczekiwany wynik wylosowania w PowerBall, to: %f %% \n', 100*wins/numSpins))
  cat(sprintf('liczba wygranych: %i \n\n', wins))
}

powerball(10000000, sample(1:69, 5), sample(1:26, 1))