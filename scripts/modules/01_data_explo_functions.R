# this script holds function needed for 01_data explo


#mean of the poly values
#this is not needed if the poly centroids where used in the extraction
mean_extract <-function(extract_results){
  l = 1
  j = 1
  poly_list = list()
  layer_list = list()
  
  for(k in 1:length(extract_results)){ 
    for(i in extract_results[[k]]){
      tmp <- mean(i)
      poly_list[[j]] <- tmp
      j = j + 1
    }
    layer_list[[l]] <- poly_list
    j = 1
    poly_list = list()
    l = l + 1 
  }
  return(layer_list)
}
#layer_list <- mean_extract(ndvi_extract)

#-------------------------------------------------------------------------------

# function to return a formatted ndvi df
# inlist is the output of mean_extract() or result of the extract job (if poly centroids were used)
# layernames is a vector with the columnnames (date stamps) of the new ndvi dt
# this also works with extraction by poly centroids
# returns a formatted data table
NDVI_DF <- function(inlist, layernames){
  ndvi_df <- data.table::data.table(PID = 1:length(inlist[[1]]))
  w = 1
  for(w in 1:length(inlist)){
    ndvi_tmp <- (unlist(inlist[[w]])-10000)/10000 #     (x-10000)/10000
    ndvi_df[,paste0(layernames[w])] <- ndvi_tmp
  }
  return(ndvi_df)
}
#-------------------------------------------------------------------------------
# this functions returns a vector with the colnames of all time stamps of a df
# so you could subset your df based on that vector and 
# cut out all columns which are date stamps

select_date_cols <- function(df, year){
  all_cols = NULL
  for(cols in colnames(df)){
    if(!is.na(stringr::str_match(cols, paste0(year,".*"))[1])){
      all_cols <- c(all_cols,cols) # all cols mit datum
    }
  }
  cols = NULL
  return(all_cols)
}

#-------------------------------------------------------------------------------
# Time series 
# function which creates a timeseries data table 
# returns a list with 1 - dt affected & 2 - dt unaffected
# input df needs colnames like: YYYYMMDD
# input is NDVI_DF output
create_timeseries <- function(ndvi_df){
  ndvi_aff <- ndvi_df %>%
    dplyr::filter(affected > 0)
  ndvi_no <- ndvi_df %>%
    dplyr::filter(affected == 0)
  time_df <-NULL
  i = NULL
  j = 1
  l = 1
  value = NULL
  end = length(ndvi_df)-1
  for(k in list(ndvi_aff,ndvi_no)){
    print(k) 
    for(i in k[,2:end]) { 
      print(j)
      tmp <- data.frame(value = i, affected = k$affected)
      time <- as.numeric(colnames(k)[j+1])
      year <- as.numeric(substr(time, 1,4))
      month <- as.numeric(substr(time, 5,6))
      day <- as.numeric(substr(time, 7,nchar(time)))
      tmp$time_raw <- as.numeric(colnames(k)[j+1])
      tmp$year <- year
      tmp$month <- month
      tmp$day <- day
      value[[j]] <- tmp
      j = j + 1
      tmp = NULL
    }
    time_tmp <- data.table::as.data.table(do.call(rbind, value))
    time_tmp$affected_factor[time_tmp$affected == "0"] <- 0
    time_tmp$affected_factor[time_tmp$affected > "0"] <- 1
    l =  l +1
    j = 1
  }
  #time_df <- data.table::as.data.table(do.call(rbind, value))
  return(time_df)
}

