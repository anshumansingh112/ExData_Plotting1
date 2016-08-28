
setwd("./exdata_data_household_power_consumption")

# --------------------------------------------------------------------------
# Reviw file:  Read 100 lines and see what it has
review <- read.csv("household_power_consumption.txt", nrows=10, sep=";")
cNames <- names(review)

#---------------------------------------------------------------------------

# Read one line at a time check and load only the rows that are requried
# this is to ensure that the program runs on low memory systems as well
consumption <- file("household_power_consumption.txt", open="r")
df <- data.frame(var1=as.character(),
                 var2=as.character(),
                 var3=as.numeric(),
                 var4=as.numeric(),
                 var5=as.numeric(),
                 var6=as.numeric(),
                 var7=as.numeric(),
                 var8=as.numeric(),
                 var9=as.numeric(),
                 stringsAsFactors = FALSE)


# Date to compare against
base_date <- as.Date(c("2007/02/01"),"%Y/%m/%d")

get_numeric <- function(x){
        
        rVal <- as.numeric(x)
        
        if(!is.numeric(rVal)){
                rVal <- c(0)
                print(paste("Not Number : ", x))
        } 
        
        rVal;
}

# Read data in blocks so that you don't use use all the memory
block_size = 10000

# Counter for which block is being read
block_id = 0   

# Index for the selected row to be inserted into the Data Frame
df_index = 0

# Loop and read the file
while(length((block <- readLines(consumption, n = block_size)) > 0)){
        
        for(line in block){
                cols <- strsplit(line, ";")
                
                if(block_id > 0){
                        
                        # print(cols)
                        v <- cols[[1]]
                        
                        d <- as.Date(v[1],"%d/%m/%Y")
                        date_index = d - base_date
                        
                        # If it is the base date or one more
                        if((date_index == 0) || (date_index == 1)){
                                
                                df_index = df_index + 1
                                
                                dstr <- paste(v[1],v[2])
                                date_time <- strptime(dstr,format = "%d/%m/%Y %H:%M:%S")
                                
                                df[df_index,1] <- as.character(v[1])
                                df[df_index,2] <- as.character(v[2])
                                df[df_index,3] <- get_numeric(v[3])
                                df[df_index,4] <- get_numeric(v[4])
                                df[df_index,5] <- get_numeric(v[5])
                                df[df_index,6] <- get_numeric(v[6])
                                df[df_index,7] <- get_numeric(v[7])
                                df[df_index,8] <- get_numeric(v[8])
                                df[df_index,9] <- get_numeric(v[9])
                                
                                # print(paste("i : ",block_id, " | DFi : ",df_index," | Time : ",date_time))
                                # print(paste("v : ", v))
                                # print(df[df_index,])
                        }
                }
        }
        
        block_id =  block_id + 1
        
        # Optimization of reading - our required data is read by Block 6 or 7
        # We can remove this break but it will increase the run time of the program
        if(block_id > 9)
                break
}

# Set the column names to the Data Frame containing the selected data
colnames(df) <- cNames

# Create date time column used by the plots later
myDate <- paste(as.Date(df$Date, "%d/%m/%Y"),df$Time)
df$DateTime <- as.POSIXct(myDate)

# ----------------------------------------------------------------------
# Plot 2
# ----------------------------------------------------------------------

fname <- paste(getwd(),"/plot2.png",sep="")
png(filename = fname,width=480,height=480,units="px")
plot(df$DateTime,
     df$Global_active_power,
     type="l",
     main="Global Active Power",
     ylab="Global Active Power (Kilowats)",
     xlab="")
dev.off()

