# ---
# Title: "Travel Audience Challenge"
# Author : "Pradeep Kuravi"
# Output :Pdf document
# Date: 17/08/2017
# ---

args <- commandArgs(TRUE)
if(length(args) == 1){
  args[2] = "output.csv"
}else if (length(args) ==0){
  print("Enter atleast input argument")
}

# Loading the data - Enter the path ----
Data = read.csv(file = args[1])

################################################################
#  First Question --
# Whether the user is active for multiple days
################################################################

# Splitting the day from the date
Data$day = format(as.Date(Data$ts,format="%Y-%m-%d"), "%d")
# Finding the unique values for the day per user so that user with 
# multiple entries in a single day wont be considered
UniquePerDay = unique(Data[c("uuid","day")])
# Getting the frequency of entries per user
UsersTable = data.frame(table(UniquePerDay$uuid))
# Finding the users with multiple day entries
UsersMultipleDays = UsersTable[UsersTable$Var1 %in% UsersTable$Var1[UsersTable$Freq > 1],1]

# Creating an empty data frame to store the final results
FinalUsers = NULL
# Listing all the unique users
FinalUsers$uuid = unique(Data$uuid)
# Writing True if user has multple day entries 
FinalUsers$multiple_days = FinalUsers$uuid %in% UsersMultipleDays

################################################################
# Second question
# Whether the user's traffic tends to occur during weekday business hours (9am to 5pm)
################################################################

# Finding the weekday based on the date
Data$Weekday = weekdays(as.Date(Data$ts,format="%Y-%m-%d"))
# Getting the hour extracted from the data
Data$Hour = as.numeric(format(strptime(Data$ts,format="%Y-%m-%d %H:%M:%S"), "%H"))
# Making a variable for the working days
workingdays = c("Monday","Tuesday","Wednesday","Thursday","Friday")
# Taking the users with entry between 9am to 5pm
tmp = Data[Data$Hour>8 & Data$Hour<17,]
# Further sorting the users who have entry during the working day
tmp = tmp[tmp$Weekday %in% workingdays,]
# Writing True if User has entry on weekday between 9am to 5pm
FinalUsers$weekday_biz = FinalUsers$uuid %in% unique(tmp$uuid)

################################################################
# Third Question
# Whether User logged from multiple location (ip address)
################################################################

# Finding the users with unique uuid and ip
UniquePerIp = unique(Data[c("uuid","ip")])
# Finding the frequency of users with entry from multiple locations (ip address)
Iptable = data.frame(table(UniquePerIp$uuid))
# Sorting the users with more than one location
IpMultipleLocations = Iptable[Iptable$Var1 %in% Iptable$Var1[Iptable$Freq > 1],1]
# Writing True if the User has entry from more than one location
FinalUsers$multiple_locations = FinalUsers$uuid %in% IpMultipleLocations


## Saving the results in csv format
write.csv(FinalUsers, file = args[2])
