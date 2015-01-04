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




# WORKING DIRECTORY -------------------------------------------------------

  directory <- '/Users/User/Dropbox/transport and conflict/Stones survey paper/'





# LOAD LIBRARIES ----------------------------------------------------------

  library(ClustOfVar)
  library(arules)
  library(arulesViz)
  library(lattice)
  library(dendextend)
  library(gmodels)
  library(qpcR)







# READ DATA ---------------------------------------------------------------

  load( paste0(directory, '/Data/analysis.RData') )
  
    # Drop 'interested in answering one last question'
    data <- data[, -grep("Interested", names(data))]
  
    # Drop PCS questions for this paper
    data <- data[, -grep("PCS", names(data))]
  






# SECTION 2 -----------------------------------------------------------


  ##  Introduction

    # Number of observations
    nrow(data)
  
    # Where are the respondents?
    sort( 100 * round(table(data$access_from_continent) / nrow(data), digits=2), decreasing = TRUE)
    length( unique(data$access_from_country) )
    sort( 100 * round(table(data$access_from_country) / nrow(data), digits=2), decreasing = TRUE)
    length( unique(data$access_from_continent) )
  
    # Survey collection dates
    summary( as.Date(data$StartDate) )
  
    # Proportion of responses taken in June
    mean( as.numeric( format(data$StartDate, '%m') ) == 6 )

    # Time to take the survey (minutes)
    summary( as.numeric(data$survey_length_of_time) )


  ##  2.1: Respondent Information

    # Affiliation (%)
    temp <- 100 * round( sort(table(data$affiliation), decreasing = TRUE) / sum(!is.na(data$affiliation)), 2)
      names(temp)[ grep("Public safety", names(temp)) ] <- "Public safety/\nlaw enforcement"
      names(temp)[ grep("Other", names(temp)) ] <- "Other"
      print(temp)
    pdf( paste0(directory, "/plots/affiliation.pdf") )
      par(mar=c(10,4,1,0))
      barplot(height=temp, ylab="% of respondents", las=2, ylim=c(0,30))
    dev.off()
    
    # Professional speciality
    temp <- 100 * round( sort(table(data$specialty), decreasing = TRUE) / sum(!is.na(data$specialty)), 2)
      names(temp)[ grep("Environmental", names(temp)) ] <- "Environmental\nprotection"
      names(temp)[ grep("Other", names(temp)) ] <- "Other"
      print(temp)
    pdf( paste0(directory, "/plots/specialty.pdf") )
      par(mar=c(7,4,1,0))
      barplot(temp, ylab="% of respondents", las=2, ylim=c(0,70))
    dev.off()


  ##  2.2: Dangerous Transport Modes

    temp <- cbind("Vulnerable\nRoad Users" = table(data$vulnerable_road_users_most_dangerous),
                  "Transport\nServices" = table(data$transport_services_most_dangerous),
                  "Motorcycles" = table(data$motorcycles_most_dangerous))

    # Weighted average
    apply(temp, 2, function(x) (x[1] + 3*x[2] + 5*x[3]) / sum(x) )

    # Dangerous transport modes
    temp <- temp[3:1, c(2,1,3)]

    pdf( paste0(directory, "/plots/dangerous_modes.pdf") )
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=c("grey50", "grey70", "grey90"), main="")
      text(x=c(.275, .56, .85), y=c(.85, .82, .8), labels=c("16", "22", "22"))
      text(x=c(.275, .56, .85), y=c(.54, .485, .34), labels=c("16", "15", "29"))
      text(x=c(.275, .56, .85), y=c(.175, .15, .02), labels=c("23", "24", "7"))
    dev.off()
    


  ##  2.3: Road Conditions

    temp <- matrix(c(22, 19, 10, 5, 4,
                     22, 17, 14, 6, 0,
                     16, 11, 22, 5, 4),
                   nrow=5, ncol=3)
      rownames(temp) <- c("Very Important", "Important", "Moderately Important", "Unimportant", "Very Unimportant")
      colnames(temp) <- c("Condition of Travelway", "Lack of Adequate Signage", "Dust from Unpaved Roads")
    temp <- temp[, c(3,1,2)]

    pdf( paste0(directory, "/plots/road_conditions_rural_crashes.pdf") )
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), main="", las=1, 
                 col=paste0("grey", 5:10*10)) 
      text(x=c(.305, .58, .86), y=c(.85, .81, .81), labels=c("16", "22", "22"))
      text(x=c(.305, .58, .865), y=c(.62, .475, .475), labels=c("11", "19", "17"))
      text(x=c(.305, .58, .865), y=c(.335, .225, .215), labels=c("22", "10", "14"))
      text(x=c(.305, .58, .865), y=c(.09, .09, .03), labels=c("5", "5", "6"))
      text(x=c(.305, .58), y=.00, labels=c("4", "4"))
    dev.off()
    
  # Weighted average
  apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )
  



  ##  2.4: Driving Behaviour
    
    temp <- cbind( "Distracted Driving" = table(data$need_research_distracted_driving),
                   "Non-Use of\nSafety Equipment" = table(data$need_research_nonuse_safety_equipment),
                   "Driving Under\nthe Influence" = table(data$need_research_DUI),
                   "Aggressive Driving" = table(data$need_research_aggressive_driving),
                   "Poor Driving" = table(data$need_research_poor_driving) )
      rownames(temp) <- c("Very Unimportant", "Unimportant", "Moderately Important", "Important", "Very Important")

    temp <- temp[ 5:1, ]
    
    pdf( paste0(directory, "/plots/driver_behaviors_that_need_research.pdf") )
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), main="", las=1, col=paste0("grey", 5:10*10)) 
      text(x=c(.255, .42, .585, .75, .915), y=c(.875, .84, .775, .75, .72), labels=temp[1,])
      text(x=c(.255, .42, .585, .75, .915), y=c(.63, .475, .4, .35, .31), labels=temp[2,])
      text(x=c(.255, .42, .585, .75, .915), y=c(.315, .2, .18, .12, .1), labels=temp[3,])
      text(x=c(.255, .42), y=c(.07, .045), labels=temp[4,1:2])
      text(x=c(.585), y=-.01, labels=temp[5,3])
    dev.off()
        
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )




  ##  2.5: Impacts on Women and Children

    temp <- cbind( table(data$crashes_disproportionately_impact_women),
                   table(data$crashes_disproportionately_impact_kids) )
      colnames(temp) <- c("Women", "Children")
    temp <- t( temp[5:1,] )
    
    pdf( paste0(directory, "/plots/women_kids_crashes.pdf") )
      par(mar=c(0,0,0,0))
      mosaicplot(temp, las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.405,.8), y=c(.91,.865), labels=temp[,1])
      text(x=c(.405,.8), y=c(.68,.53), labels=temp[,2])
      text(x=c(.405,.8), y=c(.4,.22), labels=temp[,3])
      text(x=c(.405), y=c(.15), labels=temp[1,4])
      text(x=c(.405,.8), y=c(0,0), labels=temp[,5])
    dev.off()
    
    # Weighted average
    apply(temp, 1, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )




  ##  2.6: Rural Accessibility Research Needs

    temp <- table(data$most_pressing_accessibility_issue)
      names(temp) <- c("Commercial\nActivities", "Educational\nOpportunities", "Healthcare")
    temp <- temp[c(3,1,2)]

    pdf( paste0(directory, "/plots/accessibility_issues.pdf") )
      par(mar=c(6,4,1,1))
      barplot(temp, las=2, ylab="# of respondents", ylim=c(0,40))
    dev.off()




  ##  2.7: Healthcare Accessibility Research Needs

    temp <- cbind( "General/\nPreventative" = table(data$health_access_general_preventative),
                   "Disease/\nChronic Condition" = table(data$health_access_disease_chronic_condition),
                   "Pediatric" = table(data$health_access_pediatric),
                   "Emergency/Trauma" = table(data$health_access_emergency_trauma),
                   "Maternal/\nPre-Natal Care" = table(data$health_access_maternal_prenatal))
    temp <- temp[5:1, ]
      rownames(temp) <- c("Highest Need", NA,NA,NA, "Lowest Need")
    
    pdf( paste0(directory, "/plots/healthcare_accessibility.pdf") )
      par(mar=c(1,1,1,0))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.195,.725,.91), y=c(.955,.85,.625), labels=c(2,11,23))
      text(x=c(.195,.37,.54,.725,.91), y=c(.84,.835,.75,.55,.185), labels=temp[2,])
      text(x=c(.195,.37,.54,.725,.91), y=c(.61,.625,.42,.28,.07), labels=temp[3,])
      text(x=c(.37,.54,.725), y=c(.325,.15,.09), labels=c("15","8","4"))
      text(x=c(.195,.37,.725), y=c(.19,.02,-.01), labels=c(14,4,2))
    dev.off()
    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4] + x[5]) / sum(x) )
  



  ##  2.8: Education Accessibility Research Needs

    temp <- matrix(c(3,2,0,1,0, 2,3,1,0,0, 1,4,1,0,0), nrow=5)
      rownames(temp) <- c("Very Important",
                          "Important",
                          "Moderately Important",
                          "Unimportant",
                          "Very Unimportant")
      colnames(temp) <- c("Provision of\nTransport Services",
                          "Road/Travel Safety\nEducation & Awareness for\nChildren and Parents", 
                          "Provision of\nPedestrian Facilities")
    
    pdf( paste0(directory, "/plots/education_accessibility.pdf") )
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:10*10), main="")
      text(x=c(.31,.585,.86), y=c(.75,.85,.9), labels=temp[1,])
      text(x=c(.31,.585,.86), y=c(.34,.4,.49), labels=temp[2,])
      text(x=c(.31,.585,.86), y=c(.06,.085,.085), labels=c(temp[4,1],temp[3,2:3]))
    dev.off()
    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4]) / sum(x) )




  ##  2.9: Economic Accessibility Research Needs

    temp <- matrix(c(6,8,1,0, 5,7,3,0, 1,5,8,1), nrow=4)
      rownames(temp) <- c("Strong relationship",
                          "Direct relationship",
                          "Indirect relationship",
                          "No relationship")
      colnames(temp) <- c("Connectivity to\nService-Sector Employment\nin Nearby Populated Areas", 
                          "Transport of\nAgricultural Goods to Market", 
                          "Natural Resource Extraction")

    pdf( paste0(directory, "/plots/economic_access.pdf") )
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=paste0("grey", 5:9*10), main="")
      text(x=c(.3,.575,.85), y=c(.79,.825,.95), labels=temp[1,])
      text(x=c(.3,.575,.85), y=c(.325,.425,.75), labels=temp[2,])
      text(x=c(.3,.575,.85), y=c(.015,.075,.305), labels=c(temp[3,]))
      text(.85, 0, "1")
    dev.off()
    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 4*x[2] + 3*x[3] + 2*x[4]) / sum(x) )



  ##  2.10: Rural Road Dust Research Needs
    
    pdf( paste0(directory, "/plots/road_dust.pdf") )
      par(mar=c(2.5,4,1,1))      
      barplot(table(data$inadequate_attention_dust), ylab="# of respondents", ylim=c(0,40))
    dev.off()

    table(data$inadequate_attention_dust) / sum(table(data$inadequate_attention_dust))



  ##  2.11: Rural Air Pollution Exposure

    temp <- cbind( table(data$air_pollution_roadside_residents_businesses),
                   table(data$air_pollution_individuals_in_roadway_environment),
                   table(data$air_pollution_vehicle_passengers) )
      colnames(temp) <- c("Occupants of Roadside\nResidences and Businesses",
                          "Individuals in\nRoadway Environment",
                          "Vehicle Passengers")
    temp <- temp[3:1, ]
    
    pdf( paste0(directory, "/plots/air_pollution.pdf") )
      par(mar=c(1,1,1,1))
      mosaicplot(t(temp), las=1, col=c("grey50", "grey70", "grey90"), main="")
      text(x=c(.285,.57,.85), y=c(.72,.78,.92), labels=temp[1,])
      text(x=c(.285,.57,.85), y=c(.26,.29,.73), labels=temp[2,])
      text(x=c(.285,.57,.85), y=c(.01,.0,.3), labels=temp[3,])
    dev.off()
    
    # Weighted average
    apply(temp, 2, function(x) (5*x[1] + 3*x[2] + x[3]) / sum(x) )







# SECTION 3 ---------------------------------------------------------------

  ##  This section presents hierarchical clustering and assocation rules.
  ##  Before we begin these analyses, we need to manipulate the dataset.  
  ##  StartDate and survey_length_of_time have no analytical utility, so 
  ##  I begin by dropping them:

  cluster_data <- subset(data, select = which( !(names(data) %in% c("StartDate", "survey_length_of_time")) ))


  ##  Replace NAs with 'missing' so they don't get dropped from the analysis
  #cluster_data[ is.na(cluster_data) ] <- 'missing'


  ##  Change variable names for nice plots
  cluster_data <- rename(cluster_data, c("affiliation" = "Professional Affiliation",
                                         "specialty" = "Professional Specialty",
                                         "access_from_continent" = "Access from Continent",
                                         "access_from_country" = "Access from Country",
                                         "vulnerable_road_users_most_dangerous" = "Vulnerable Road Users Most Dangerous",
                                         "transport_services_most_dangerous" = "Transport Services Most Dangerous",
                                         "motorcycles_most_dangerous" = "Motorcycles Most Dangerous",
                                         "most_pressing_accessibility_issue" = "Most Pressing Accessibility Issue",
                                         "inadequate_attention_dust" = "Inadequate Attention to Dust",
                                         "air_pollution_individuals_in_roadway_environment" = "Air Pollution: Individuals in Roadway Environment at Risk",
                                         "air_pollution_roadside_residents_businesses" = "Air Pollution: Roadside Residents & Businesses at Risk",
                                         "air_pollution_vehicle_passengers" = "Air Pollution: Vehicle Passengers at Risk",
                                         "need_research_condition_of_travelway" = "Need Research (Road): Condition of Travelway & Rural Crashes",
                                         "need_research_lack_of_adequate_signage" = "Need Research (Road): Dust from Unpaved Roads & Rural Crashes",
                                         "need_research_dust_from_unpaved_roads" = "Need Research (Road): Aggressive Driving & Rural Crashes",
                                         "need_research_aggressive_driving" = "Need Research (Driver): Aggressive Driving & Rural Crashes",
                                         "need_research_DUI" = "Need Research (Driver): DUIs & Rural Crashes",
                                         "need_research_distracted_driving" = "Need Research (Driver): Distracted Driving & Rural Crashes",
                                         "need_research_nonuse_safety_equipment" = "Need Research (Driver): Non-Use of Safety Equipment & Rural Crashes",
                                         "need_research_poor_driving" = "Need Research (Driver): Poor Driving & Rural Crashes",
                                         "education_need_research_safety_education_awareness" = "Need Research (Education): Safety Education & Awareness for Children & Parents",
                                         "education_need_provision_of_pedestrian_facilities" = "Need Research (Education): Provision of Pedestrian Facilities",
                                         "education_need_research_provision_of_transport_services" = "Need Research (Education): Provision of Transport Services",
                                         "crashes_disproportionately_impact_women" = "Rural Road Crashes Disproportionately Impact Women",
                                         "crashes_disproportionately_impact_kids" = "Rural Road Crashes Disproportionately Impact Children",
                                         "economic_access_agriculture2market" = "Road Safety and Transport of Agricultural Goods to Market",
                                         "economic_access_labor2service_sector" = "Road Safety and Connectivity to Service-Sector Employment",
                                         "economic_access_natural_resource_extraction" = "Road Safety and Natural Resource Extraction",
                                         "health_access_maternal_prenatal" = "Need Research (Healthcare): Maternal/Pre-Natal",
                                         "health_access_general_preventative" = "Need Research (Healthcare): General/Preventative",
                                         "health_access_pediatric" = "Need Research (Healthcare): Pediatric",
                                         "health_access_disease_chronic_condition" = "Need Research (Healthcare): Disease/Chronic Condition",
                                         "health_access_emergency_trauma" = "Need Research (Healthcare): Emergency/Trauma"))


  ##  HIERARCHICAL CLUSTERING

# don't include country where the survey was taken
# the algorithm crashes when we include it
hclust <- hclustvar(X.quali = cluster_data) #discrete_data[, -grep("country", names(discrete_data))])

table(cluster_data$"Access from Continent",
      cluster_data$"Need Research (Education): Provision of Pedestrian Facilities")




pdf( paste0(directory, 'plots/cluster.pdf'), height=20, width=20 )
  par(mar = c(0,4,0,0))
  plot(hclust, main ="")
dev.off()


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






# ANALYSIS ----------------------------------------------------------------

  
  


  # Hierarchical clustering
  # don't include country where the survey was taken
  # the algorithm crashes when we include it
  hclust <- hclustvar(X.quali = data[, -grep("country", names(data))])
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






# Bivariate relationships
CrossTable(data$research_school_pedestrian, data$access_from_continent, chisq=T)
table(data$access_goods2market)

table(data$specialty, data$need_research_DUI)
table(data$need_research_aggressive_driving, data$inadequate_attention_dust)
table(data$need_research_DUI, data$specialty)




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

