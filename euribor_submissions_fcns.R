# Function to replace column names depending on sheet format

replace_columns <- function(euribor.df){
  new.columns <- c("Bank","X1W","X2W","X1M","X2M","X3M","X6M","X9M","X12M")
  old.columns <- c("Bank","X1W","X2W","X3W","X1M","X2M","X3M","X4M","X5M","X6M","X7M","X8M","X9M","X10M","X11M","X12M")
  out <- euribor.df
  
  if(length(colnames(euribor.df)) == 9){
    names(out) <- new.columns
    return(out)
  } else if(length(colnames(euribor.df)) == 16){
    names(out) <- old.columns
    return(out)
  } else {
    cat(euribor.df[1,1])
    stop("Unknown sheet format")
  }
}

clean_worksheet_names <- function(worksheet.names){
  out <- gsub("[[:punct:]]", "", worksheet.names)
  out.1 <- gsub(" ", "", out, fixed = TRUE)
  out.2 <- ifelse(nchar(out.1) == 3, paste(0,out.1,sep=""), out.1)
  
  return(out.2)
}

extract_euribor_worksheet <- function(file.name){
    
  # Load workbook and read sheets into a list. Standardise column names.
  wb <- XLConnect::loadWorkbook(file.name)
  lst <- XLConnect::readWorksheet(wb, sheet = getSheets(wb))
  attributes(lst)$file <- file.name
  attributes(lst)$year = str_extract(file.name, "20[0-9][0-9]")
  
  # Return list  
  return(lst)
}

clean_euribor_worksheet <- function(lst){
  lst.mod <- lapply(lst, FUN=function(x) names(x) = replace_columns(x))
  
  # Create date variable from list names
  wb.dated <- Map(function(x, y) mutate(x, Date = as.Date(paste(substr(y,1,2),"/",substr(y,3,4),"/", attributes(lst)$year, sep=""), format="%d/%m/%Y")), lst.mod, clean_worksheet_names(names(lst.mod)))
  
  # Collapse workbook list into a single data.frame
  wb.dated.df <- do.call("rbind", wb.dated)
  
  # Return data.frame
  return(wb.dated.df) 
}

# Download a set of Euribor data files from a character vector of URLs

download_euribor_files <- function(urls, directory.name){
  # Grab everything after the final slash as file name
  file.names <- str_extract(urls, "([^/]+$)")
  
  # Loop through URL vector and download all files
  for(i in seq(1,length(urls))){
    download.file(url = urls[i], destfile = paste(directory.name,"/",file.names[i], sep = ""))
  }
}

# Read a bunch of Euribor data files in a directory
read_euribor_files <- function(directory.name){
  # Grab contents of directory; save to vector
  directory.contents <- dir(directory.name)
  
  # Extract all worksheets in the directory into a list
  out <- lapply(directory.contents, FUN=function(x) extract_euribor_worksheet(paste(directory.name,"/",x, sep = "")))
  
  # Return the list
  return(out)
}

# Clean Euribor files, i.e. standardise columns
clean_euribor_files <- function(lst){
  out <- lapply(lst, FUN = function(x) clean_euribor_worksheet(x))
  return(out)
}

# Checks and documents sheets that don't follow the correct format.
check_bad_sheets <- function(data.lst){
  out <- data.frame()
  k <- 1
  for (i in seq(1,length(data.lst))){
    for(j in seq(1,length(data.lst[[i]]))){
      if(dim(data.lst[[i]][[j]])[2] != 9 && dim(data.lst[[i]][[j]])[2] != 16){
        out[k,1] <- attributes(data[[i]])$names[j]
        out[k,2] <- attributes(data[[i]])$year
        out[k,3] <- attributes(data[[i]])$file
        out[k,4] <- i
        out[k,5] <- j
        k <- k + 1
      } 
    }
  }
  colnames(out) <- c("sheet","year","file","file.no","sheet.no")
  return(out)
}


