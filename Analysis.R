##############################################################
##                                                          ##
##    Survey: Rural Transport Issues for Future Research    ##
##                                                          ##
##               Steven Jones, sjones@eng.ua.edu            ##
##             Joe Walsh, jwalsh@community-inc.com          ##
##                                                          ##
##                      September 1, 2014                   ##
##                                                          ##
##############################################################


##  This script conducts the analysis.  It loads the data, 
##  reports descriptive statistics, performs hierarchical 
##  cluster analysis, and verifies the model (including 
##  choice of distance function) using cross validation.



## LOAD LIBRARIES
library(ClustOfVar)
library(arules)
library(arulesViz)
library(lattice)
library(dendextend)


## LOAD DATA
data <- read.csv("/home/ubuntu/modified_data using joint ranks.csv", header = TRUE) #, na.strings = "missing")

  # Drop 'interested in answering one last question'
  data <- data[, -grep("Interested", names(data))]
  data <- data[, -grep("length", names(data))]

  # Drop PCS questions for this paper
  data <- data[, -grep("PCS", names(data))]



  # # Survey continent and country
  # URL <- paste('http://www.iplocation.net/index.php?query=',
  #              data$IP.Address,
  #              sep='')
  # webpages <- getURL(url = URL)
  # 
  # data$continent <- sapply(X=webpages, FUN=function(x) as.character(readHTMLTable(x)[[13]][7,2]))
  # data$country <- sapply(X=webpages, FUN=function(x) as.character(readHTMLTable(x)[[12]][3,2]))
  # 
  # round(x=table(data$continent) / nrow(data), 
  #       digits=2)
  # 
  # round(x=table(data$country) / nrow(data), 
  #       digits=2)


  # Survey collection dates
  summary( as.Date(data$StartDate) )
    
  # Time to take the survey (minutes)
  summary( as.numeric(data$survey_length_of_time) )
    
  # Good participation (% that agreed to answer a few questions)
  100 * round( mean( as.character(data$Interested_in_answering_one_last_question_) == 'Yes', na.rm = TRUE), 2)

  # Bivariate relationships
  table(data$access.from.continent, data$research.school.pedestrian)
  table(data$access.goods2market)

  table(data$specialty, data$need.research.DUI)
  table(data$need.research.aggressive.driving, data$inadequate.attention.dust)
  table(data$need.research.DUI, data$specialty)

  # Hierarchical clustering
  # don't include country where the survey was taken
  # the algorithm crashes when we include it
  hclust <- hclustvar(X.quali = data[, -c(21)])
  stability(hclust, B=25)
  cutree(hclust, k=3)

  cutreevar(hclust, k=3)
  pdf("dendrogram.pdf")
    plot(hclust, main="Cluster Analysis Dendrogram")
  dev.off()



rules <- apriori(data, parameter = list(supp = 0.025, conf = 1))
options(digits=2)




  # Market basket analysis 
  rules <- sort(rules, by='lift', decreasing=TRUE)
  inspect(rules[1:30])





## DESCRIPTIVE STATISTICS

  ## Data in different format (e.g. most, neutral, least are separate variables)
  data <- read.csv("/home/ubuntu/modified_data.csv") 

  # Drop 'interested in answering one last question'
  data <- data[, -grep("Interested", names(data))]
  data <- data[, -grep("length", names(data))]
  
  # Drop PCS questions for this paper
  data <- data[, -grep("PCS", names(data))]


  # Affiliation (%)
  temp <- 100 * round( sort(table(data$affiliation), decreasing = TRUE) / sum(!is.na(data$affiliation)), 2)
    names(temp)[ grep("Public safety", names(temp)) ] <- "Public safety/\nlaw enforcement"
    names(temp)[ grep("Other", names(temp)) ] <- "Other"
  pdf("affiliation.pdf")
    par(mar=c(10,4,1,0))
    barplot(height=temp, ylab="% of respondents", las=2)
  dev.off()

  # Professional speciality
  temp <- 100 * round( sort(table(data$specialty), decreasing = TRUE) / sum(!is.na(data$specialty)), 2)
    names(temp)[ grep("Environmental", names(temp)) ] <- "Environmental\nprotection"
    names(temp)[ grep("Other", names(temp)) ] <- "Other"
  pdf("specialty.pdf")
    par(mar=c(8,4,1,0))
    barplot(temp, ylab="% of respondents", las=2)
  dev.off()


  # Access from country (%)
  length( unique(data$access.from.country) )
  100 * round( sort(x = table(data$access.from.country), decreasing = TRUE) / length(data$access.from.country), 2)


  # Access from continent (%)
  length( unique(data$access.from.continent) )
  100 * round( sort(x = table(data$access.from.continent), decreasing = TRUE) / length(data$access.from.continent), 2)


  # Dangerous transport modes
  temp <- cbind( most_dangerous = table(data$most.dangerous.mode),
              dangerous = table(data$dangerous.mode),
              least_dangerous = table(data$least.dangerous.mode) )
  temp <- temp[c(2,3,1),]
  pdf("dangerous_modes.pdf")
    par(mar=c(1,1,1,1))
    mosaicplot(temp, las=1, col=c("grey50", "grey70", "grey90"), main="")
    text(x=c(.28, .57, .85), y=c(.85, .8, .775), labels=c("16", "22", "22"))
    text(x=c(.28, .57, .85), y=c(.55, .5, .35), labels=c("16", "15", "29"))
    text(x=c(.28, .57, .85), y=c(.20, .175, .03), labels=c("23", "24", "7"))
  dev.off()

  # Weighted average
  apply(temp, 1, function(x) (5*x[1] + 3*x[2] + x[3]) / sum(x) )




  # Road conditions and rural road crashes
  temp <- matrix(c(22, 19, 10, 5, 4,
                22, 17, 14, 6, 0,
                16, 11, 22, 5, 4),
              nrow=5, ncol=3)
    rownames(temp) <- c("very important", "important", "moderately important", "unimportant", "very unimportant")
    colnames(temp) <- c("Travelway", "Signage", "Dust")
    temp <- temp[,c(3,1,2)]
    pdf("road_conditions_rural_crashes.pdf")
      mosaicplot(t(temp), main="", las=1, 
                 col=paste0("grey", 5:10*10)) 
      text(x=c(.335, .6, .865), y=c(.85, .8, .8), labels=c("16", "22", "22"))
      text(x=c(.335, .6, .865), y=c(.625, .475, .475), labels=c("11", "17", "19"))
      text(x=c(.335, .6, .865), y=c(.35, .225, .215), labels=c("22", "14", "10"))
      text(x=c(.335, .6, .865), y=c(.09, .09, .04), labels=c("5", "6", "5"))
      text(x=c(.335, .6), y=.00, labels=c("4", "4"))
    dev.off()


    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )


    
    # Driver behaviors that need research
    temp <- cbind( aggressive.driving = table(data$need.research.aggressive.driving),
                distracted.driving = table(data$need.research.distracted.driving),
                DUI = table(data$need.research.DUI),
                nonuse.safety.equipment = table(data$need.research.nonuse.safety.equipment),
                educate.regulate = table(data$need.research.education.regulation) )
      rownames(temp) <- c("Important", "Moderately important", "Unimportant", "Very important", "Very unimportant")
      colnames(temp) <- c("Aggressive driving", 
                       "Distracted driving",
                       "DUI",
                       "Non-Use\nEquipment",
                       "Educate regulate")

    temp <- temp[ c(4,1,2,3,5), c(2,4,3,1,5) ]

    pdf("driver_behaviors_that_need_research.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), main="", las=1, col=paste0("grey", 5:10*10)) 
      text(x=c(.26, .42, .585, .75, .915), y=c(.875, .84, .775, .75, .72), labels=temp[1,])
      text(x=c(.26, .42, .585, .74, .915), y=c(.65, .475, .4, .35, .32), labels=temp[2,])
      text(x=c(.26, .42, .585, .74, .915), y=c(.32, .2, .18, .12, .1), labels=temp[3,])
      text(x=c(.26, .42), y=c(.07, .05), labels=temp[4,1:2])
      text(x=c(.585), y=0, labels=temp[5,3])
    dev.off()

    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )



    # Women, kids, and crashes
    temp <- cbind( table(data$crashes.disproportionately.impact.women),
                   table(data$crashes.disproportionately.impact.kids) )
      colnames(temp) <- c("Women", "Children")
    temp <- t( temp[c(4,1,3,2,5),] )

    pdf("women_kids_crashes.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(temp, las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.425,.8), y=c(.9,.85), labels=temp[,1])
      text(x=c(.425,.8), y=c(.68,.525), labels=temp[,2])
      text(x=c(.425,.8), y=c(.4,.225), labels=temp[,3])
      text(x=c(.425), y=c(.165), labels=temp[1,4])
      text(x=c(.425,.8), y=c(0,0), labels=temp[,5])
    dev.off()

    # Weighted average
    apply(temp, 1, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )



    # Most pressing accessibility issue
    temp <- table(data$most.pressing.accessibility)
      names(temp) <- c("Commercial\nactivities", "Educational\nopportunities", "Healthcare")
      temp <- temp[c(3,1,2)]
    pdf("accessibility_issues.pdf")
      par(mar=c(6,4,1,1))
      barplot(temp, las=2, ylab="# of respondents", ylim=c(0,40))
    dev.off()



    # Healthcare accessibility
    temp <- cbind( table(data$Most.important.health.access),
                   table(data$Important.health.access),
                   table(data$Neutral.health.access),
                   c(table(data$Unimportant.health.access)[1:2],
                     "Maternal care" = 0,
                     table(data$Unimportant.health.access)[3:4]),
                   table(data$Least.important.health.access) )
      colnames(temp) <- c("Most important", NA,NA,NA, "Least important")
  
    temp <- temp[ c(2,1,4,5,3), ]

    pdf("healthcare_accessibility.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(temp, las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.215,.73,.91), y=c(.95,.85,.6), labels=c(2,11,23))
      text(x=c(.215,.38,.55,.73,.91), y=c(.85,.84,.775,.55,.2), labels=temp[,2])
      text(x=c(.215,.38,.55,.73,.91), y=c(.6,.62,.4,.28,.07), labels=temp[,3])
      text(x=c(.38,.55,.73), y=c(.35,.175,.1), labels=c("15","8","4"))
      text(x=c(.215,.38,.73), y=c(.2,.025,0), labels=c(14,4,2))
    dev.off()

    # Weighted average
    apply(temp, 1, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )




    # Education accessibility
    temp <- matrix(c(2,3,1,0,0, 1,4,1,0,0, 3,2,0,1,0), nrow=5)
      rownames(temp) <- c("Very important",
                          "Important",
                          "Moderately important",
                          "Unimportant",
                          "Very unimportant")
      colnames(temp) <- c("Safety education", "Pedestrian facilities", "Transport services")
    temp <- temp[, c(3,1,2) ]

    pdf("education_accessibility.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.3,.575,.85), y=c(.75,.85,.9), labels=temp[1,])
      text(x=c(.3,.575,.85), y=c(.375,.45,.55), labels=temp[2,])
      text(x=c(.3,.575,.85), y=c(.06,.1,.1), labels=c(temp[4,1],temp[3,2:3]))
    dev.off()
    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4]) / sum(x) )



    # Access
    temp <- matrix(c(5,7,3,0, 6,8,1,0, 1,5,8,1), nrow=4)
    rownames(temp) <- c("Strong relationship",
                        "Direct relationship",
                        "Indirect relationship",
                        "No relationship")
    colnames(temp) <- c("Goods to market", "Labor to market", "Access natural resources")
    temp <- temp[, c(2,1,3) ]

    pdf("access.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:9*10), main="")
      text(x=c(.28,.575,.85), y=c(.78,.825,.95), labels=temp[1,])
      text(x=c(.28,.575,.85), y=c(.35,.45,.75), labels=temp[2,])
      text(x=c(.28,.575,.85), y=c(.01,.075,.325), labels=c(temp[3,]))
      text(.85, 0, "1")
    dev.off()

    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4]) / sum(x) )



    # Road dust
    pdf("road_dust.pdf")
      par(mar=c(2.5,4,1,1))      
      barplot(table(data$inadequate.attention.dust), 
              ylab="frequency")
    dev.off()
    table(data$inadequate.attention.dust) / sum(table(data$inadequate.attention.dust))



    # Air pollution
    temp <- cbind( table(data$air.pollution.most.vulnerable),
                   table(data$air.pollution.vulnerable),
                   table(data$air.pollution.least.vulnerable) )
      colnames(temp) <- c("Most vulnerable", "Vulnerable", "Least vulnerable")
    temp <- temp[ c(1,3,2), ]

    pdf("air_pollution.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(temp, las=1, col=c("grey50", "grey70", "grey90"), main="")
      text(x=c(.28,.57,.85), y=c(.72,.78,.91), labels=temp[,1])
      text(x=c(.28,.57,.85), y=c(.28,.32,.73), labels=temp[,2])
      text(x=c(.28,.57,.85), y=c(.01,.0,.31), labels=temp[,3])
    dev.off()

    # Weighted average
    apply(temp, 1, function(x) (5*x[1] + 3*x[2] + x[3]) / sum(x) )



#     # PCS
#     temp <- cbind( table(data$PCS.building.roads),
#                    table(data$PCS.transport.provision) )
#       colnames(temp) <- c("Build roads", "Transport provision")
#     temp <- temp[ c(5,1,2,4,3), ]
# 
#     par(mar=c(1,1,1,1))
#     mosaicplot(t(temp), las=1, col=paste0("grey", 5:10*10), main="")
#     text(x=c(.39,.8), y=c(.78,.74), labels=temp[1,])
#     text(x=c(.39,.8), y=c(.42, .33), labels=temp[2,])
#     text(x=c(.39,.8), y=c(.16,.1), labels=temp[3,])
# 
#     # Weighted average
#     apply(temp, 1, function(x) (5*x[1] + 3*x[2] + x[3]) / sum(x) )



# # PCS
# table(data$PCS.building.roads, data$PCS.transport.provision)
# pcs.rules <- apriori(subset(data, select=grep('PCS', names(data))), parameter = list(supp = 0.05, conf = 0.8))
# inspect( sort(pcs.rules, by='lift', decreasing=T) )









prop <- function(x){
  
  temp.variable <- data[, x]
  
  # point estimates
  temp.table <- table(temp.variable) 
  point.estimates <- round(temp.table / sum(temp.table), 2)
  
  # standard deviations
  bootstrapped.proportions <- matrix(NA, 1000, length( unique(temp.variable) ) - 1)
  for(i in 1:1000){
    bootstrap.sample <- sample(temp.variable, length(temp.variable), replace = TRUE)
    bootstrapped.proportions[i,] <- round(table(bootstrap.sample) / sum( !is.na(bootstrap.sample) ), 2)
  }

  # Return the numbers
  return( list( n = sum( !is.na(temp.variable) ),
                point.estimates = point.estimates,
                min.95 = round( apply(bootstrapped.proportions, 2, function(x) quantile(x, probs=.025)), 2),
                max.95 = round( apply(bootstrapped.proportions, 2, function(x) quantile(x, probs=.975)), 2)
                ) 
          )
  
}


names(two_level_data)
names(three_level_data)



# Which is most dangerous?
par(mar=c(4,6,3,1), las=1)
plot(c(0,1), c(1,3), 
     type='n', axes = FALSE,
     main = '', xlab = 'proportion', ylab = '')

  par(cex=2)

  t <- prop('vulnerable_users_most_dangerous')
  points(t$point.estimates[1], 1, pch="-")
  points(t$point.estimates[2], 1, pch="o")
  points(t$point.estimates[3], 1, pch="+")

  u <- prop('buses_most_dangerous')
  points(u$point.estimates[1], 2, pch="-")
  points(u$point.estimates[2], 2, pch="o")
  points(u$point.estimates[3], 2, pch="+")

  v <- prop('motorcycles_most_dangerous')
  points(v$point.estimates[1], 3, pch="-")
  points(v$point.estimates[2], 3, pch="o")
  points(v$point.estimates[3], 3, pch="+")

t <- subset(data, 
            select=c('vulnerable_users_most_dangerous',
                     'buses_most_dangerous',
                     'motorcycles_most_dangerous'))
View(t)
  par(cex=1)
  legend('topright', 
         pch = c("+", "o", "-"), 
         legend = c("Most Dangerous", "Dangerous", "Least Dangerous"))
  axis(1)
  axis(2, at = 1:3, labels = c(paste0("motorcycles\n(", t$n, ")"),
                               paste0("public\ntransportation\n(", u$n, ")"), 
                               paste0("vulnerable\nusers\n(", v$n, ")")) )
     


prop('air_pollution_vulnerable_users')
prop('inadequate_attention_dust')
prop('air_pollution_vehicle_passengers')

lapply(names(three_level_data), FUN=prop)

names(five_level_data)


itemFrequencyPlot(data)
View(data)  


