pricer <- function(infile, outfile, targetsize){
  input <- read.table(infile, fill = TRUE, stringsAsFactors = FALSE)
  colnames(input) <- c("timestamp", 'type', "order-id", "side", "price", "size")
  totalBidSize <- 0
  totalAskSize <- 0
  bidBook <- data.frame()
  askBook <- data.frame()
  #for(i in 1:nrow(input)){
  for(i in 1:3){
    if(input[i,'type'] == 'A'){
      if(input[i, 'side'] == 'B'){
        bidBook <- rbind(bidBook,input[i,c(3,5,6)])
        print(bidBook)
        print("bidBook")
      }
      if(input[i, 'side'] == 'S'){
        askBook <- rbind(askBook,input[i,c(3,5,6)])
        print(askBook)
        print("askBook")
      }
    #这里少了一个排序方法
    }
    if(input[i, 'type'] == 'R'){
      id <- input[i, 'order-id']
      size <- input[i, 'size']
      Reduce(id, bidBook, size)
      Reduce(id, askBook, size)
    }
    
  }




}


Reduce <- function(id, book, size){
  for(i in 1:nrow(book)){
    if (book[i, "order-id"] == id){
      book[i, "size"] <-  book[i, "size"] - size
      if(is.na(book[i, "size"])){
        book <- book[-i,]
      }
      print(book[i, ])
    }
  }
}
sortBook <- function(book) 
pricer("sample1_in.txt", 1,0)

