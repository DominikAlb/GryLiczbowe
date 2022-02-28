library(httr)
library(jsonlite)
library("rjson")
library(RJSONIO)


#dlugosc gry
N <- 100000.0
cat(sprintf("N: %f", N), file = "out.txt", sep = "\n")
#Pobiera tablice losowych liczb do testu
getRandom <- function (min, max, count) {
  result <- numeric()
  x <- c("https://qrng.anu.edu.au/API/jsonI.php?length=",255,"&type=uint8")
  x <- paste(x, collapse = "")
  res <- fromJSON(x)
  arr <- res[['data']] %% max+1
  arr <- append(arr, result)
  arr <- arr[!duplicated(arr)]
  for (i in arr) {
    if (i >= min && i <= max) {
      result <- append(result, i)
    }
    if (length(result) == count) {
      break
    }
  }
  return(result)
}
#pobiera 30 losowych liczb z zakresu [0, 36]
#getRandom(0, 36, 30)

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
  #cat(sprintf("Oczekiwany wynik wylosowania w ruletce: %i , to: %.10f %% \n", number, (100*wins/numSpins)))
  #cat(sprintf("wygrana wynosi %i \n", budget - startedBudget))
  #cat(sprintf('liczba wygranych: %i \n\n', wins))

  return((budget - startedBudget)/startedBudget)
}

arr <- replicate(100, euroRoulette(N,N, getRandom(0, 36,1), 1))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla ruletki to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)

lotto <- function(numSpins, numbers) {
  wins <- 0
  for (i in 1:numSpins) {
    count <- 0
    outcome <- sample(1:49, 6)

    for (i in 1:6) {
      if (!is.na(match(numbers[i], outcome))) {
        count <- count + 1
      }
    }

    if (count == 6) {
      wins <- wins + 1
    }
  }

  #cat(sprintf('Losowane liczby: '))
  #for (i in numbers) {
  #  cat(sprintf('%i ', i))
  #}
  #cat(sprintf('\n Oczekiwany wynik wylosowania w lotto, to: %f %% \n', 100*wins/numSpins))
  #cat(sprintf('liczba wygranych: %i \n\n', wins))

  return(wins/numSpins)
}

arr <- replicate(100, lotto(N, getRandom(1, 49, 6)))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla lotto to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)

multiMulti <- function(budget, numSpins, numbers) {
  wins <- 0.0
  for (i in 1:numSpins) {
    if (budget <= 0) break
    else { budget <- budget - 1 }
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
  #cat(sprintf('Losowane liczby: '))
  #for (i in numbers) {
  #  cat(sprintf('%i ', i))
  #}
  #cat(sprintf('\n Oczekiwany wynik wylosowania w Multi Multi, to: %f %% \n', 100*wins/numSpins))
  #cat(sprintf('liczba wygranych: %i \n\n', wins))
}

arr <- replicate(100, multiMulti(N, N, getRandom(1, 80, 10)))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla multi multi 10 z 10 to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)


arr <- replicate(100, multiMulti(N, N, getRandom(1, 80, 5)))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla multi multi 5 z 10 to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)



arr <- replicate(100, multiMulti(N, N, getRandom(1, 80, 1)))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla multi multi 1 z 10 to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)

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
  #cat(sprintf('Losowane liczby: '))
  #for (i in numbers5) {
  #  cat(sprintf('%i ', i))
  #}
  #cat(sprintf(' i '))
  #for (i in numbers2) {
  #  cat(sprintf('%i ', i))
  #}
  #cat(sprintf('\n Oczekiwany wynik wylosowania w EuroJackpot, to: %f %% \n', 100*wins/numSpins))
  #cat(sprintf('liczba wygranych: %i \n\n', wins))
}

arr <- replicate(100, euroJackpot(N, getRandom(1, 50, 5), getRandom(1, 10, 2)))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla euroJackpot to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)

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
  #cat(sprintf('Losowane liczby: '))
  #for (i in numbers5) {
  #  cat(sprintf('%i ', i))
  #}
  #cat(sprintf(' i '))
  #for (i in numbers) {
  #  cat(sprintf('%i ', i))
  #}
  #cat(sprintf('\n Oczekiwany wynik wylosowania w PowerBall, to: %f %% \n', 100*wins/numSpins))
  #cat(sprintf('liczba wygranych: %i \n\n', wins))
}

arr <- replicate(100, powerball(N, getRandom(1, 69, 5), getRandom(1, 26, 1)))
sd <- sd(arr)
mean <- mean(arr)
cat(sprintf('Oczekiwany rezultat dla powerball to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3)), file = "out.txt", append = TRUE)