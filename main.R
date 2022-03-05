library(httr)
library(jsonlite)
library("rjson")
library(RJSONIO)
library(lgr)


#dlugosc gry
N <- 100000.0
#powtorzenia
M <- 400
lgr$info("N: %f", N)
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

statisctics <- function(mean, sd) {
  x <- seq(-N*12, N*12, by = 1000)
  y <- dnorm(x, mean = mean, sd = sd)
  plot(x,y)
}

euroRoulette <- function(numSpins, number, budget, bet, debug) {
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

  if (debug) {
    cat(sprintf("Oczekiwany wynik wylosowania w ruletce: %i , to: %.10f %% \n", number, (100*wins/numSpins)))
    cat(sprintf("wygrana wynosi %i \n", budget - startedBudget))
    cat(sprintf('liczba wygranych: %i \n\n', wins))
  }
  return((budget - startedBudget)/startedBudget)
}

arr <- replicate(M, euroRoulette(N, getRandom(0, 36,1), N, 1, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla ruletki to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)

lotto <- function(numSpins, numbers, budget, bet, debug) {
  wins <- 0
  startedBudget <- budget
  for (x in 1:numSpins) {
    if (budget <= 0) break
    else { budget <- budget - bet }

    count <- 0
    outcome <- sample(1:49, 6)

    for (y in 1:6) {
      if (!is.na(match(numbers[y], outcome))) {
        count <- count + 1
      }
    }
    #https://www.lotto.pl/lotto/jak-grac#ile-mozna-wygrac
    if (count == 6) {
      wins <- wins + 1
      budget <- budget + 2000000
    } else if (count == 5) {
      budget <- budget + 5300
    } else if (count == 4) {
      budget <- budget + 170
    } else if (count == 3) {
      budget <- budget + 24
    }
  }
  if (debug) {
    cat(sprintf('Losowane liczby: '))
    for (i in numbers) {
      cat(sprintf('%i ', i))
    }
    cat(sprintf('\n Oczekiwany wynik wylosowania w lotto, to: %f %% \n', 100*wins/numSpins))
    cat(sprintf("wygrana wynosi %i \n", budget - startedBudget))
    cat(sprintf('liczba wygranych: %i \n\n', wins))
  }

  return((budget - startedBudget)/startedBudget)
}

arr <- replicate(M, lotto(N, getRandom(1, 49, 6), N*3, 3, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla lotto to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)

multiMulti <- function(numSpins, numbers, budget, bet, debug) {
  winnerMatrix <- matrix(c(
    4.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 16.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 2.00, 54.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 2.00, 8.00, 84.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 4.00, 20.00, 700.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 2.00, 8.00, 120.00, 1300.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 2.00, 4.00, 20.00, 200.00, 6000.00, 0.00, 0.00, 0.00,
    0.00, 0.00, 0.00, 4.00, 20.00, 60.00, 600.00, 22000.00, 0.00, 0.00,
    0.00, 0.00, 0.00, 2.00, 8.00, 42.00, 300.00, 2000.00, 70000.00, 0.00,
    0.00, 0.00, 0.00, 2.00, 4.00, 12.00, 140.00, 520.00, 10000.00, 250000.00), nrow = 10)
  wins <- 0.0
  startedBudget <- budget
  for (x in 1:numSpins) {
    if (budget <= 0) break
    else { budget <- budget - bet }
    count <- 0
    outcome <- sample(1:80, 20)

    for (y in seq_along(numbers)) {
      if (!is.na(match(numbers[y], outcome))) {
        count <- count + 1
      }
    }

    if (count == length(numbers)) {
      wins <- wins + 1
    }
    if (count != 0) {
      ###cat(sprintf('err Losowane liczby: %f :-: %f ----\n', count, length(numbers)))
      ###cat(sprintf('winerr2: %f ----\n', winnerMatrix[count, length(numbers)]))
      budget <- budget + winnerMatrix[length(numbers), count]
    }
  }
  if (debug) {
    cat(sprintf('Losowane liczby: '))
    for (i in numbers) {
      cat(sprintf('%i ', i))
    }
    cat(sprintf('\n Oczekiwany wynik wylosowania w Multi Multi, to: %f %% \n', 100*wins/numSpins))
    cat(sprintf("wygrana wynosi %i \n", budget - startedBudget))
    cat(sprintf('liczba wygranych: %i \n\n', wins))
  }
  return((budget - startedBudget)/startedBudget)
}

arr <- replicate(M, multiMulti(N, getRandom(1, 80, 10), N*12.5, 12.5, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla multi multi 10 z 10 to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)

arr <- replicate(M, multiMulti(N, getRandom(1, 80, 5), N*2.5, 2.5, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla multi multi 5 z 10 to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)

arr <- replicate(M, multiMulti(N, getRandom(1, 80, 1), N*2.5, 2.5, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla multi multi 1 z 10 to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)

euroJackpot <- function (numSpins, numbers5, numbers2, budget, bet, debug) {
  euro <- 4.70
  winnerMatrix <- matrix(c(0,0,9.9,
                           0,7.3,19.8,
                           14.1,15.5,54.9,
                           101.4,209.6,4115.8,
                           118183.4,334853.1,24000400.1), nrow = 3,ncol=5)
  wins <- 0
  startedBudget <- budget
  for (x in 1:numSpins) {
    if (budget <= 0) break
    else { budget <- budget - bet }
    count <- 0
    count2 <- 0
    outcome <- sample(1:50, 5)
    outcome2 <- sample(1:10, 2)
    for (y in 1:5) {
      if (!is.na(match(numbers5[y], outcome))) {
        count <- count + 1
      }
    }
    for (y in 1:2) {
      if (!is.na(match(numbers2[y], outcome2))) {
        count2 <- count2 + 1
      }
    }

    if (count + count2 == 7) {
      wins <- wins + 1
    }
    if (count != 0) {
      ###cat(sprintf('err Losowane liczby: %f :-: %f ----\n', count, count2+1))
      ###cat(sprintf('winerr2: %f ----\n', winnerMatrix[count2+1, count]))
      budget <- budget + winnerMatrix[count2+1, count]*euro
    }
  }
  if (debug) {
    cat(sprintf('Losowane liczby: '))
    for (i in numbers5) {
      cat(sprintf('%i ', i))
    }
    cat(sprintf(' i '))
    for (i in numbers2) {
      cat(sprintf('%i ', i))
    }
    cat(sprintf('\n Oczekiwany wynik wylosowania w EuroJackpot, to: %f %% \n', 100*wins/numSpins))
    cat(sprintf("wygrana wynosi %f \n", budget - startedBudget))
    cat(sprintf('liczba wygranych: %i \n\n', wins))
  }

  return((budget - startedBudget)/startedBudget)
}

arr <- replicate(M, euroJackpot(N, getRandom(1, 50, 5), getRandom(1, 10, 2), N*12.5, 12.5, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla euroJackpot to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)

powerball <- function (numSpins, numbers5, number, budget, bet, debug) {
  winnerMatrix <- matrix(c(0, 4,
                           4, 7,
                           7, 100,
                           100, 50000,
                           1000000, 70000000), nrow = 2,ncol=5)
  wins <- 0
  startedBudget <- budget
  for (x in 1:numSpins) {
    if (budget <= 0) break
    else { budget <- budget - bet }
    count <- 0
    count2 <- 0
    outcome <- sample(1:69, 5)
    outcome2 <- sample(1:26, 1)
    for (y in 1:5) {
      if (!is.na(match(numbers5[y], outcome))) {
        count <- count + 1
      }
    }
    if (number == outcome2) {
      count2 <- count2 + 1
    }

    if (count + count2 == 6) {
      wins <- wins + 1
    }
    if (count != 0) {
      ###cat(sprintf('err Losowane liczby: %f :-: %f ----\n', count, count2+1))
      ###cat(sprintf('winerr2: %f ----\n', winnerMatrix[count2+1, count]))
      budget <- budget + winnerMatrix[count2+1, count]
    }
  }
  if (debug) {
    cat(sprintf('Losowane liczby: '))
    for (i in numbers5) {
      cat(sprintf('%i ', i))
    }
    cat(sprintf(' i '))
    for (i in numbers) {
      cat(sprintf('%i ', i))
    }
    cat(sprintf('\n Oczekiwany wynik wylosowania w PowerBall, to: %f %% \n', 100*wins/numSpins))
    cat(sprintf("wygrana wynosi %i \n", budget - startedBudget))
    cat(sprintf('liczba wygranych: %i \n\n', wins))
  }

  return((budget - startedBudget)/startedBudget)
}

arr <- replicate(M, powerball(N, getRandom(1, 69, 5), getRandom(1, 26, 1), N*2, 2, FALSE))
sd <- sd(arr)
mean <- mean(arr)
lgr$info('Oczekiwany rezultat dla powerball to: %.3f %% +/- %.3f %% z 95%% pewnoscia\n\n', round(100*mean, 3), round(100*1.96*sd, 3))
statisctics(mean, sd)