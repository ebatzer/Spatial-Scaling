csv_address <- "" # read from this address
output_address <- "" # write to this address
raw_csv <- read_csv(csv_address, col_names = FALSE)
chunk_len <- 17
chunks_to_process <- nrow(raw_csv)/chunk_len

accumulator <- data.frame()

for (i in 1:chunks_to_process) {
  # creates a dataframe for the chunk from the CSV
  working_chunk <- data.frame(raw_csv[(chunk_len*(i-1)+2):(chunk_len*i),])
  names(working_chunk) <- raw_csv[chunk_len*(i-1)+1,]
  
  # strips columns that are labeled NA
  NA_cols <- is.na(names(working_chunk))
  if (sum(NA_cols) > 0) {
    NA_col_ind <- which(is.na(names(working_chunk)))
    working_chunk <- working_chunk[-NA_col_ind]
  }
  
  # fill in blank rows of columns 1:3
  working_chunk[2:chunk_len-1,1:3] <- working_chunk[1,1:3]
  
  # joins chunk to rest of already processed data
  accumulator <- dplyr::bind_rows(accumulator, working_chunk)
}

write.csv(accumulator, file = output_address)