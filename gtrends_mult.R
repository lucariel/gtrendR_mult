library(tidyverse)
library(gtrendsR)


gtrends_multi_interest_over_time<-function(keyword = NA, geo = "", time = "today+5-y",
                                           gprop = c("web", "news", "images", "froogle", "youtube"),
                                           category = 0, hl = "en-US", low_search_volume = FALSE,
                                           cookie_url = "http://trends.google.com/Cookies/NID", tz = 0,
                                           onlyInterest = FALSE, pivot_keyword = keyword[1]){
  
  if(length(keyword)<=5){
    tb<-gtrends(keyword, geo, time,gprop, category, hl, low_search_volume, cookie_url, tz, onlyInterest)
    tb$interest_over_time
  }
  if(length(keyword)>5){
    keywords <- keyword[keyword!=pivot_keyword] 
    iter<-seq(from=  1, to  = length(keywords), by = 4)
    
    crr_tb<-gtrends(keyword =  c(pivot_keyword,keywords[1],keywords[2],keywords[3],keywords[4]), geo, time,gprop, category, hl, low_search_volume, cookie_url, tz, onlyInterest)
    ddf<-crr_tb$interest_over_time[0,]
    ctrl_<-crr_tb$interest_over_time[0,]
    
    
    for(i in iter){
      kw <- c(pivot_keyword,keywords[i],keywords[i+1],keywords[i+2],keywords[i+3])
      kw<-kw[!is.na(kw)]
      tb<-gtrends(keyword = kw, geo, time,gprop, category, hl, low_search_volume, cookie_url, tz, onlyInterest)
      
      a<-crr_tb$interest_over_time %>% filter(keyword == pivot_keyword)
      b<-tb$interest_over_time %>% filter(keyword == pivot_keyword)
      
      corr<-mean(a$hits)/mean(b$hits)
      
      tb$interest_over_time$hits = tb$interest_over_time$hits*corr 
      ctrl_a<-tb$interest_over_time %>% filter(keyword == pivot_keyword)
      if(i != 1){
        tb$interest_over_time<-tb$interest_over_time %>% filter(keyword != pivot_keyword)
      }
      ctrl_a['c'] = i
      ddf<-rbind(tb$interest_over_time,ddf)
      ctrl_<-rbind(ctrl_a,ctrl_)
      
    }
    ctrl_$c = as.factor(ctrl_$c)
    list(ddf,ctrl_)
  }
}
