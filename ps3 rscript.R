print(log(2,base=1.02))
print(log(2,base=1.04))
(1+0.04)^20
(1+0.00)^10*(1+0.08)^10
(1+0.03)^20
library(pwt10)
library(tidyverse)
pwt = select(pwt10.0,'year','country','rgdpna','rkna','emp','hc')
pwt_short = pwt %>% filter(year==1981|year==1991|year==2001|year==2011|year==2019) %>% drop_na()
pwt_short <- pwt_short %>% mutate(dif_years = 10)
                                  
pwt_short <- pwt_short %>% mutate(
growth_Y = ((lead(rgdpna)/rgdpna)^(1/dif_years))-1,
growth_K = ((lead(rkna)/rkna)^(1/dif_years))-1,
growth_L = ((lead(emp)/emp)^(1/dif_years))-1) 
  
pwt_short <- pwt_short %>% mutate(y = rgdpna/emp, k = rkna/emp)
pwt_short <- pwt_short %>% mutate(
  growth_y = ((lead(y)/y)^(1/dif_years))-1,
  growth_k = ((lead(k)/k)^(1/dif_years))-1,
  growth_h = ((lead(hc)/hc)^(1/dif_years))-1 )
pwt_short <- pwt_short %>% mutate(alpha = 1/3,growth_A = growth_y - alpha*growth_k - (1-alpha)*growth_h,log_y=log(y))
view(pwt_short)
pdf("plot1.pdf")
ggplot(data=pwt_short%>%filter(year==1981),aes(x=log(y),y=growth_y))+
  geom_point()+geom_smooth(method=lm)+labs(x=r"(log(y) at start year)",y=r"(average growth_y)")+theme_bw()
