pricer <- function(infile, outfile, targetsize) {
  input <- read.table(infile, fill = TRUE, stringsAsFactors = FALSE)
  #input data as table
  colnames(input) <- c("timestamp", "type", "order-id", "side", "price", "size")
  #label the data
  totalBidSize <- 0 #record the size of Bidbook
  totalAskSize <- 0 #record the size of Askbook
  income <- 0 #record the income
  expense <- 0 #record the expense
  bidBook <- data.frame()
  askBook <- data.frame()
  for (i in 1:nrow(input)) {
    if (input[i, "type"] == "A") {
      if (input[i, "side"] == "B") {
        bidBook <- rbind(bidBook, input[i, c(3, 5, 6)]) #put id, price and size into the book
        bidBook <- bidBook[order(-bidBook$price), ] #descending sort the bidbook
      }
      if (input[i, "side"] == "S") {
        askBook <- rbind(askBook, input[i, c(3, 5, 6)])
        askBook <- askBook[order(askBook$price), ] #ascending sort the askbook
      }
    }
    if (input[i, "type"] == "R") {
      id <- input[i, "order-id"]
      size <- as.numeric(input[i, 4]) #convert size from string to number
      bidBook = Reduce(id, bidBook, size) #update the bidbook
      askBook = Reduce(id, askBook, size) #update the askbook
    }
    totalBidSize <- sumSize(bidBook) #calculate the total size of bidbook
    totalAskSize <- sumSize(askBook) #calculate the total size of askbook


    if (totalBidSize >= targetsize) {
      calculate <- calculating(bidBook, targetsize) #calculate potential income
      if (calculate != income) {
        income <- calculate
        report(input[i, "timestamp"], "S", income, outfile)
      }
    } else { #not satisfy the size
      if (income > 0) {
        report(input[i, "timestamp"], "S", 0, outfile) #if previous income is above 0, report NA
        income <- 0
      }
    }
    if (totalAskSize >= targetsize) {
      calculate <- calculating(askBook, targetsize) #calculate potential expense
      if (calculate != expense) {
        expense <- calculate
        report(input[i, "timestamp"], "B", expense, outfile)
      }
    } else { #not satisfy the size
      if (expense > 0) {
        report(input[i, "timestamp"], "B", 0, outfile) #if previous expense is above 0, report NA
        expense <- 0
      }
    }
  }
}


Reduce <- function(id, book, size) {
  for (i in 1:nrow(book)) {
    if (nrow(book)!=0) {
      if (book[i, "order-id"] == id) {
        book[i, "size"] <- (book[i, "size"]) - size #reduce the size of present record
        if (is.na(book[i, "size"])) { #size has been reduced to NA
          book <- book[-i, ] #delete this record
        }
      }
    }
  }
  return(book)
}

sumSize <- function(book) {
  if (!is.null(book[1, "order-id"])) {
    totalSize <- sum(book[, "size"]) #sum up all sizes in the book
    return(totalSize)
  }
  return(0)
}

calculating <- function(book, targetsize) {
  total <- 0
  row <- 1
  while (targetsize > 0 & !is.na(book[row, "size"])) {
    #loop in the situation that still has target and not empty
    presentSize = book[row, "size"]
    if (presentSize < targetsize) { #need next record
      total <- total + presentSize * book[row, "price"]
      targetsize <- targetsize - presentSize
    } else { #end in this record
      total <- total + targetsize * book[row, "price"]
      targetsize <- 0
    }
    row <- row + 1
  }
  return(total)
}

report <- function(timestamp, side, price, output) {
  if (price == 0) { #in the special situation that NA reporting
    cat(file = output, append = TRUE, timestamp, side, "NA \n")
    print(paste(timestamp, side, "NA")) #preview without formating
  } else { #report the correspondent value and format them
    cat(file = output, append = TRUE, timestamp, side, format(price, nsmall = 2),
      "\n")
    print(paste(timestamp, side, price)) #preview without formating
  }
}
