library(dplyr)
library(CausalImpact)
library(zoo)
library(bigrquery)
library(bigQueryR)
library(googleAuthR)
library(dplyr)
library(plotly)
library(tidyr)
library(drc)
library(timetk)
library(highcharter)
library(shinydashboard)
library(shiny)
library(plotly)
require(shinydashboard)
library(dplyr)
library(DT)
library(scales)
library(highcharter)
library(shinyjs)
library(config)
#library(sodium)
library(tibbletime)
library(dplyr)
library(lubridate)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(prophet)
library(gsheet)
library(dygraphs)
library(dplyr)
library(drc)
library(ggExtra)
library(ggplotify)

### run first the Non Elastic SKUs
### the purpose of this script is to filter via a loop a list of non elastic SKUs 
### then create a syntetic control method by randomly assigned the non elastic SKUs to groups.

Sys.setenv("IP_DIRECTORY_PATH"=getwd())
local_path <- Sys.getenv("IP_DIRECTORY_PATH")

data_lookup <- read_csv(paste0(local_path, "/data_look_up.csv")) ### non elastic culsterd skus
df_brand <- read_csv(paste0(local_path, "/df_brand.csv")) ### dataframe with sales data
data_lookup <- data_lookup %>%filter(corr_quantity< -0.2) ### take the highest inelastic
### Exclude MKP

df_brand <- df_brand %>% filter(grepl('CDS', sku))
count_sku <- unique(df_brand$sku)
count_sku <- as.data.frame(count_sku)

### Exlude outliers
df_brand <- df_brand %>% filter(gmv<1000000)
count_sku <- unique(df_brand$sku)
count_sku <- as.data.frame(count_sku)


#### filter data from list in Dplyr
id <- data_lookup$s_iterator ### create the list 
ddd <- df_brand %>% filter(sku%in%id) ### use in operator

count_sku <- unique(ddd$sku)
count_sku <- as.data.frame(count_sku)

### list of unique SKUs
sku_u <- unique(ddd$sku)
sku_u <- as.data.frame(sku_u)
row <- nrow(sku_u)

r <- 0:1000
counter = 0
mape = 0
lift = 0


find_optimal_mape <- function(r){
  for (iter in r){

### random sampling unique SKU's method by assign 0 and 1 for test and control label
    
    a<-sample(0:1, row, rep = TRUE)
    sku_u$label<-a
    sku_u$sku<-sku_u$sku_u
    
    ### join them by unique sku to have the right label of clustering method
    ddd <- inner_join(ddd, sku_u, by = "sku")  
    #write.csv(ddd, file="causal_impact_3_cent.csv")
    
    
    #### let's build the two time series
    d.control <- ddd %>% filter(label==0)
    d.test <- ddd %>% filter(label==1)
    
    
    d.control <- d.control %>% group_by(order_date)%>%summarise(
      gmv=sum(gmv))
    
    d.test <- d.test %>% group_by(order_date)%>%summarise(
      gmv=sum(gmv))
    
   
    ### if error
    
    try(error <- rlang::last_error(),silent=TRUE)
    try(error_m <- error$message, silent=TRUE)
    try(log <- grepl('data', error_m), silent=TRUE)
    try(if(log==TRUE){
      d.test = d.test[-1,]
    }else{NULL},silent=TRUE)
    
    
     
    #d.control$gmv_test <- d.test$gmv ### put all into one df
    
    d.control$y <- d.control$gmv 
    d.control$x1  <- d.test$gmv
    
   
    ### outlier filter
    #d.control <- d.control %>% dplyr::filter(gmv < quantile(gmv, 0.98))
    
    
    #### data prep and cleaning for causal impact
    d.control <- d.control %>% filter(order_date<"2022-09-28")
    
    df <- d.control
    time.points <- seq.Date(as.Date("2022-04-01"), by = 1, length.out = 180)
    attach(df)
    data <- zoo(cbind(y, x1), time.points)
    head(data)
    
    #test <- data 
    #test <- as.data.frame(test)
    
    #### let's go with causal impact bro
    
    matplot(data, type = "l")
    pre.period <- as.Date(c("2022-04-01", "2022-08-15"))
    post.period <- as.Date(c("2022-08-16", "2022-09-23"))
    
    impact <- CausalImpact(data, pre.period, post.period)
    
    
    ### model features MAPE
    mape<-(impact$summary$Actual[1]-impact$summary$Pred[1])/impact$summary$Actual[1]*100
    
    ##ddd total opportuinty sizing
    #sum(ddd$level_1_discount)/181
    
    ### get the relative effect
    lift<-impact$summary$RelEffect[1]
    
    ### build data frame from causal impact object for the plots
    series_df <- impact[["series"]]
    series_df <- as.data.frame(series_df)
   
    
    if( mape> 0 && mape < 1 ){
      summary(impact, "report")
      write.csv(ddd, file="causal_impact_1_cent.csv")
      write_csv(series_df, file = "series_df.csv")
      print("I am inside if condition")
      print(mape)
      print(counter)
      print(lift)
      plot(impact)
      
      break
    }
    else{
      print("I am inside Else block")
      remove(a)
      remove(sku_u)
      remove(ddd)
      remove(d.control)
      remove(d.test)
      print(counter)
      print(mape)
    }
    
    counter = counter+1
    
  }
  
}


find_optimal_mape(r)

