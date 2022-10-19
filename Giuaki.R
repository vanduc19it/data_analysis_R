#1
# import read dataset

equipment <- read.csv(file = 'C:/Users/acer/OneDrive/Documents/R-ChuyenDe/giuaki/equipment_Ukraine_Russia_War.csv')
add_data <- read.csv(file = 'C:/Users/acer/OneDrive/Documents/R-ChuyenDe/giuaki/add_data.csv')
View(equipment)
View(add_data)
# show all colname in dataset
colnames(equipment)
#tom tat tap du lieu

summary(equipment)

a1 <-sum(equipment$helicopter) / sum(equipment$tank+equipment$helicopter+equipment$drone + equipment$naval.ship  ) * 100

a2<-sum(equipment$tank) / sum(equipment$tank+equipment$helicopter+equipment$drone + equipment$naval.ship ) * 100

a3<-sum(equipment$drone) / sum(equipment$tank+equipment$helicopter+equipment$drone + equipment$naval.ship ) * 100

a4<-sum(equipment$naval.ship) / sum(equipment$tank+equipment$helicopter+equipment$drone + equipment$naval.ship ) * 100

a5 <-sum(equipment$helicopter)*1
a6<-sum(equipment$tank)
a7<-sum(equipment$drone)
a8<-sum(equipment$naval.ship)


data= matrix(c(a1, a2, a3, a4), ncol=4, byrow=TRUE)
colnames(data) <- c('tank','helicopter','drone','ship')

quanity<- c(a5, a6,a7,a8)
data1 <- rbind(data, quanity)
data1
rownames(data) <- c('percent')

final=as.table(data)
final


head(df)
#tinh tong cac row ko day du
# show number of row and col
nrow(equipment)
  ncol(equipment)

#min(equipment$tank)

#tinh trung binh cot bat ki
#median(equipment$tank)
#mean(equipment$tank)


#as.numeric as.character

#add lib
library(dplyr)

#2
#date: ngày xảy ra war giữa UKraina với Russia
#day: thứ tự ngày từ ngày 1...đến ngày cuối
#aircraft: số lượng phi cơ sử dụng
#helicopter: số lượng máy bay trực thăng sử dụng
#tank: so lương xe tang
#APC:Armored Personnel Carrier: số lượng xe bọc thép sử dụng
#field artillery: số lượng pháo dã chiến sử dụng
#MRL:Multiple Rocket Launcher: số lượng hệ thống bệ phóng tên lửa sử dụng
#military auto: số lượng ô tô quân sự sử dụng
#fuel tank: số lượng thùng nhiên liệu sử dụng
#drone: số lượng drone sử dụng
#naval ship: số lượng tàu hải quân sử dụng
#anti-aircraft warfare:số lượng hệ thống phòng không sử dụng
#special equipment: số lượng thiết bị đặc biệt đc sử dụng
#mobile SRBM system:số lượng tên lữa đạn đạo tầm ngắn sử dụng
#greatest losses direction: nơi tổn thất lớn nhất
#vehicles and fuel tanks: xe và thùng nhiên liệu sử dụng
#cruise missiles:số lượng tên lữa hành trình sử dụng


#3
#replace all NA by 0
equipment[is.na(equipment)] <- "0"
View(equipment)

#remove all rows contain NA
# careful sẽ xóa hết data luôn nếu row nào cx có NA.
#gsub() thay the 
equipment <- as.data.frame(apply(equipment, 2, function(x) gsub("\\s+", "", x)))
equipment[equipment == ""] <- NA
#tail() la show ra n rows cuoi cung
tail(equipment)
#na.omit loai bo tat ca cac truogn hop ko day du (NA)
equipment <- na.omit(equipment)
tail(equipment)

#4
# Merge and clean two datasets
# Add new columm died and injured by merge two datasets together
# merge function: Merge two data frames by common columns or row names

mergeDataset <- merge(add_data, equipment, by = "date", all.x=TRUE,all.y=TRUE)
# by merge two dataset by use common columns  "date"
#all.x=TRUE, all.y=TRUE: To retain all values of the first dataset and second dataset

# complete.cases() return a vector logic cho biet cases nao complete
# Keep only complete rows
mergeDataset <- mergeDataset[complete.cases(mergeDataset), ] 
View(mergeDataset)

#5

#  find top 5 area most area by died quantity
top5died <- head(mergeDataset[order(mergeDataset$died, decreasing = TRUE),], 5)
top5died[c(2, 18)]

# find top 5 area most area by injured quantity
top5injured <- head(mergeDataset[order(mergeDataset$injured, decreasing = TRUE),], 5)
top5injured[c(3, 18)]

#(head(): show ra n rows dau tien)
#the most died days
day1 <- head(mergeDataset[order(mergeDataset$died, decreasing = TRUE),], 1)
day1[c(1, 2)]
#at least died days
day2 <- head(mergeDataset[order(mergeDataset$died, decreasing = FALSE),], 1)
day2[c(1, 2)]
#the most injured days
day3 <- head(mergeDataset[order(mergeDataset$injured, decreasing = TRUE),], 1)
day3[c(1, 3)]
#at least injured days
day4 <- head(mergeDataset[order(mergeDataset$injured, decreasing = FALSE),], 1)
day4[c(1, 3)]

#total number of died
sum(mergeDataset$died)

#total number of injured
sum(mergeDataset$injured)

#select any col 
mergeDataset%>%select(helicopter)

#total helicopter used


#total aircraft used
sum(mergeDataset$aircraft)

# total number of injured of each day and add to dataset
mergeDataset <- mutate(mergeDataset, total = died + injured)

# area died more than 10000 people
mergeDataset[mergeDataset$died >= 10001, c(1,2, 18)]

# area injured more than 100000 people
mergeDataset[mergeDataset$injured >= 100001, c(1,3, 18)]

#bieudo
hist(mergeDataset$died)
hist(mergeDataset$injured)

