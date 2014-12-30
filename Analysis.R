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
library(gmodels)





# READ DATA ---------------------------------------------------------------

load('/Users/User/Dropbox/transport and conflict/Stones survey paper/Data/analysis.RData')

  # Drop 'interested in answering one last question'
  data <- data[, -grep("Interested", names(data))]

  # Drop PCS questions for this paper
  data <- data[, -grep("PCS", names(data))]






# DESCRIBE DATA -----------------------------------------------------------


  # Where are the respondents?
  sort( 100 * round(table(data$access_from_continent) / nrow(data), digits=2), decreasing = TRUE)
  sort( 100 * round(table(data$access_from_country) / nrow(data), digits=2), decreasing = TRUE)


  # Survey collection dates
  summary( as.Date(data$StartDate) )
  

  # Time to take the survey (minutes)
  summary( as.numeric(data$survey_length_of_time) )
  

  # Bivariate relationships
  CrossTable(data$research_school_pedestrian, data$access_from_continent, chisq=T)
  table(data$access_goods2market)

  table(data$specialty, data$need_research_DUI)
  table(data$need.research_aggressive_driving, data$inadequate_attention_dust)
  table(data$need.research_DUI, data$specialty)

  # Hierarchical clustering
  # don't include country where the survey was taken
  # the algorithm crashes when we include it
  names(data) <- c("Professional Affiliation",
                   "Professional Specialty",
                   "Dangerous Transport Modes",
                   "Need Research: Travelway Condition",
                   "Need Research: Roadway Signage",
                   "Need Research: Dust from Unpaved Roads",
                   "Need Research: Aggressive Driving",
                   "Need Research: DUI",
                   "Need Research: Distracted Driving",
                   "Need Research: Non-Use of Safety Equipment",
                   "Need Research: Poor Driving",
                   "Transport and Healthcare Accessibility",
                   "Need Research: Road/Travel Safety Education",
                   "Need Research: Provision of Pedestrian Facilities",
                   "Need Research: Provision of Transport Services",
                   "Transport Agricultural Goods to Market",
                   "Connectivity to Service-Sector Employment",
                   "Natural Resource Extraction",
                   "Inadequate Attention to Dust/Air Pollution",
                   "Rank Groups Most Vulnerable to Air Pollution",
                   "Access from Country",
                   "Access from Continent")
  hclust <- hclustvar(X.quali = data[, -c(21)])
  stability(hclust, B=25)
  cutree(hclust, k=3)

  cutreevar(hclust, k=3)
  pdf("dendrogram.pdf")
    par(mar=c(1,8,0,1))
    plot(hclust, main="", ylab="") 
    mtext("                                                                           height", side=2, line=2.4)
    mtext('Cluster Analysis Dendrogram', side=2, font=2, cex=1.5, line=5)
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
    par(mar=c(7,3,1,0))
    barplot(temp, ylab="% of respondents", las=2)
  dev.off()


  # Access from country (%)
  length( unique(data$access.from.country) )
  100 * round( sort(x = table(data$access.from.country), decreasing = TRUE) / length(data$access.from.country), 2)


  # Access from continent (%)
  length( unique(data$access.from.continent) )
  100 * round( sort(x = table(data$access.from.continent), decreasing = TRUE) / length(data$access.from.continent), 2)

  levels(data$most.dangerous.mode) <- c("Motorcycles", "Transport Services", "Vulnerable Users")
  levels(data$dangerous.mode) <- c("Motorcycles", "Transport Services", "Vulnerable Users")
  levels(data$least.dangerous.mode) <- c("Motorcycles", "Transport Services", "Vulnerable Users")

  # Dangerous transport modes
  temp <- cbind( "Most Dangerous" = table(data$most.dangerous.mode),
              "Dangerous" = table(data$dangerous.mode),
              "Least Dangerous" = table(data$least.dangerous.mode) )
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
    rownames(temp) <- c("Very Important", "Important", "Moderately Important", "Unimportant", "Very Unimportant")
    colnames(temp) <- c("Condition of Travelway", "Lack of Adequate Signage", "Dust from Unpaved Roads")
    temp <- temp[,c(3,1,2)]
    pdf("road_conditions_rural_crashes.pdf")
      par(mar=c(1,1,1,1))
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
      rownames(temp) <- c("Important", "Moderately Important", "Unimportant", "Very Important", "Very Unimportant")
      colnames(temp) <- c("Aggressive Driving", 
                       "Distracted Driving",
                       "Driving Under\nthe Influence",
                       "Non-Use of\nSafety Equipment",
                       "Poor Driving")

    temp <- temp[ c(4,1,2,3,5), c(2,4,3,1,5) ]

    pdf("driver_behaviors_that_need_research.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), main="", las=1, col=paste0("grey", 5:10*10)) 
      text(x=c(.26, .42, .585, .75, .915), y=c(.875, .84, .775, .75, .72), labels=temp[1,])
      text(x=c(.26, .42, .585, .75, .915), y=c(.65, .475, .4, .35, .32), labels=temp[2,])
      text(x=c(.26, .42, .585, .75, .915), y=c(.32, .2, .18, .12, .1), labels=temp[3,])
      text(x=c(.26, .42), y=c(.07, .05), labels=temp[4,1:2])
      text(x=c(.585), y=-.01, labels=temp[5,3])
    dev.off()

    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )



    # Women, kids, and crashes
    temp <- cbind( table(data$crashes.disproportionately.impact.women),
                   table(data$crashes.disproportionately.impact.kids) )
      colnames(temp) <- c("Women", "Children")
    temp <- t( temp[c(4,1,3,2,5),] )

    pdf("women_kids_crashes.pdf")
      par(mar=c(0,0,0,0))
      mosaicplot(temp, las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.41,.8), y=c(.9,.85), labels=temp[,1])
      text(x=c(.41,.8), y=c(.68,.525), labels=temp[,2])
      text(x=c(.41,.8), y=c(.4,.225), labels=temp[,3])
      text(x=c(.41), y=c(.165), labels=temp[1,4])
      text(x=c(.41,.8), y=c(0,0), labels=temp[,5])
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
    temp <- temp[ c(2,1,4,5,3), ]
    rownames(temp) <- c("General/\nPreventative", "Disease/\nChronic Condition", 
                        "Pediatric", "Emergency/Trauma", "Maternal/\nPre-Natal")
    colnames(temp) <- c("Most Important", NA,NA,NA, "Least Important")
    
    pdf("healthcare_accessibility.pdf")
      par(mar=c(1,1,1,0))
      mosaicplot(temp, las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.215,.73,.91), y=c(.95,.85,.625), labels=c(2,11,23))
      text(x=c(.215,.38,.55,.73,.91), y=c(.85,.84,.75,.55,.185), labels=temp[,2])
      text(x=c(.215,.38,.55,.73,.91), y=c(.595,.625,.42,.28,.07), labels=temp[,3])
      text(x=c(.38,.55,.73), y=c(.325,.15,.09), labels=c("15","8","4"))
      text(x=c(.215,.38,.73), y=c(.2,.02,-.01), labels=c(14,4,2))
    dev.off()

    # Weighted average
    apply(temp, 1, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )




    # Education accessibility
    temp <- matrix(c(2,3,1,0,0, 1,4,1,0,0, 3,2,0,1,0), nrow=5)
      rownames(temp) <- c("Very Important",
                          "Important",
                          "Moderately Important",
                          "Unimportant",
                          "Very Unimportant")
      colnames(temp) <- c("Road/Travel Safety\nEducation & Awareness for\nChildren and Parents", 
                          "Provision of\nPedestrian Facilities", 
                          "Provision of\nTransport Services")
    temp <- temp[, c(3,1,2) ]

    pdf("education_accessibility.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.31,.585,.86), y=c(.75,.85,.9), labels=temp[1,])
      text(x=c(.31,.585,.86), y=c(.34,.415,.49), labels=temp[2,])
      text(x=c(.31,.585,.86), y=c(.06,.09,.09), labels=c(temp[4,1],temp[3,2:3]))
    dev.off()
    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4]) / sum(x) )





    # Access
    temp <- matrix(c(5,7,3,0, 6,8,1,0, 1,5,8,1), nrow=4)
    rownames(temp) <- c("Strong relationship",
                        "Direct relationship",
                        "Indirect relationship",
                        "No relationship")
    colnames(temp) <- c("Transport of\nAgricultural Goods to Market", 
                        "Connectivity to\nService-Sector Employment\nin Nearby Populated Areas", 
                        "Natural Resource Extraction")
    temp <- temp[, c(2,1,3) ]

    pdf("access.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:9*10), main="")
      text(x=c(.3,.575,.85), y=c(.79,.825,.95), labels=temp[1,])
      text(x=c(.3,.575,.85), y=c(.325,.425,.75), labels=temp[2,])
      text(x=c(.3,.575,.85), y=c(.015,.075,.305), labels=c(temp[3,]))
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
    rownames(temp) <- c("Occupants of Roadside\nResidences and Businesses",
                        "Individuals in\nRoadway Environment",
                        "Vehicle Passengers")
      colnames(temp) <- c("Most Vulnerable", "Vulnerable", "Least Vulnerable")
    temp <- temp[ c(1,3,2), ]

    pdf("air_pollution.pdf")
      par(mar=c(1,1,1,1))
      mosaicplot(temp, las=1, col=c("grey50", "grey70", "grey90"), main="")
      text(x=c(.285,.57,.85), y=c(.72,.78,.92), labels=temp[,1])
      text(x=c(.285,.57,.85), y=c(.26,.29,.73), labels=temp[,2])
      text(x=c(.285,.57,.85), y=c(.01,.0,.3), labels=temp[,3])
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

