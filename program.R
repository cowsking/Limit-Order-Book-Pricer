pricer <- function(infile, outfile, targetsize){
  input <- read.table(infile, fill = TRUE, stringsAsFactors = FALSE)
  colnames(input) <- c("timestamp", 'type', "order-id", "side", "price", "size")
  totalBidSize <- 0
  totalAskSize <- 0
  income <- 0
  expense <- 0
  bidBook <- data.frame()
  askBook <- data.frame()
  for(i in 1:nrow(input)){
    if(input[i,'type'] == 'A'){
      if(input[i, 'side'] == 'B'){
        bidBook <- rbind(bidBook,input[i,c(3,5,6)])
        bidBook <- bidBook[order(-bidBook$price), ]
        #print(bidBook)
        #print("bidBook")
      }
      if(input[i, 'side'] == 'S'){
        askBook <- rbind(askBook,input[i,c(3,5,6)])
        askBook <- askBook[order(askBook$price), ]
        #print(askBook)
       
      }
    }
    if(input[i, 'type'] == 'R'){
      id <- input[i, 'order-id']
      size <- input[i, 'size']
      bidBook = Reduce(id, bidBook, size)
      askBook = Reduce(id, askBook, size)
    }
    totalBidSize <- sumSize(bidBook)
    totalAskSize <- sumSize(askBook)

    
    if(totalBidSize >= targetsize){
      calculate <- calculating(bidBook, targetsize)
      if(calculate!= income){
        income <- calculate
        report(input[i, "timestamp"],"S", income, outfile)
      }
    }
    else{
      if(income > 0){
        report(input[i, "timestamp"],"S", 0, outfile)
        income <- 0
      }
    }
    
    if(totalAskSize >= targetsize){
      calculate <- calculating(askBook, targetsize)
      if(calculate!= expense){
        expense <- calculate
        report(input[i, "timestamp"],"B", expense, outfile)
      }
    }
    else{
      if(expense > 0){
        report(input[i, "timestamp"],"B", 0, outfile)
        expense <- 0
      }
    }
  }



}


Reduce <- function(id, book, size){
  for(i in 1:nrow(book)){
    if(!is.na(book[i, "size"])){
      if (book[i, "order-id"] == id){
        book[i, "size"] <-  book[i, "size"] - size
        if(is.na(book[i, "size"])){
          book <- book[-i,]
        }
        # print(book[i, ])
      }
    }
  }
  return(book)
}

sumSize <- function(book){
  if(!is.null(book[1, "order-id"])){
    #print(book[, "size"])
    totalSize <- sum(book[, "size"])
    return(totalSize)
  }
  return(0)
}

calculating <- function(book, targetsize){
  total <- 0
  row <- 1
  while(targetsize > 0 & !is.na(book[row, "size"])){
    presentSize = book[row, "size"]
    if(presentSize < targetsize){
      total <- total + presentSize*book[row, "price"]
      targetsize <- targetsize - presentSize
    }
    else{
      total <- total + targetsize*book[row, "price"]
      targetsize <- 0
    }
   #print(total)
    row <- row + 1
  }
  return(total)
}

report <- function(timestamp,side, price, output){
  if(price == 0){
    cat(file=output, append = TRUE, timestamp, side, "NA\n")
    print(paste(timestamp, side, "NA"))
  }
  else{
    cat(file=output, append = TRUE, timestamp, side, format(price, nsmall=2), "\n")
    print(paste(timestamp, side, price))
  }
}

pricer("sample1_in.txt", "test_out.txt", 1)

