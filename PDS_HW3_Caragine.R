######################### PDS HW 3 ##################################
## Author: Crystal Caragine
## Date: 2/26/2020
## Class: Political Data Science
## Saved As: PDS_HW3_Caragine


library(ggplot2)

##################### Question 1 

primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPollssuper<-primaryPolls[primaryPolls$state %in% c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", 
                                                                "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"), ]
primaryPollssuper<-primaryPollssuper[primaryPollssuper$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]


ggplot(data=primaryPollssuper)+
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name, 
                            linetype=candidate_name)) +
  facet_wrap(~ state, nrow=5) +
  ggtitle("Super Tuesday") +
  labs(y="Percent", x = "Date") +
  labs(colour = "Candidate") +
  theme_minimal()





