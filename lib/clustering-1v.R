country_import <- read.csv("../data/country_cleaned.csv")

countries = as.character(unique(country_import$Country))
years = rep(1996:2016,5)
commodities = c(rep("Coffee",21),rep("Tea",21),rep("Spices",21),rep("Chocolate",21),rep("Cocoa",21))

df <- data.frame(matrix(ncol = length(countries)+ 2, nrow = length(years), data = 0))
colnames(df) <- c("Year","Commodity",countries)
df$Year = years
df$Commodity = commodities


for(j in 1996:2016){
  temp1 = subset(country_import, Year == j)
    for(k in 1:length(countries)){
      for(i in 1:5){
      temp2 = subset(temp1, Country == countries[k])
      df[21*(i-1)+j-1995,2+k] = ifelse(is.na(temp2[1,i+2]),0,temp2[1,i+2])
    }
  }
}

k = 11
kmeans(t(df[,3:dim(df)[2]]),k)[1]


