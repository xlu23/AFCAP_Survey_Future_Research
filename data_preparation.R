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


##  This script prepares the data survey data for analysis.  It
##  loads the data, adds new variables and transforms existing
##  variables, drops unnecessary variables, and saves the 
##  results in a CSV file.



## LOAD LIBRARIES
library(RCurl)
library(XML)
library(plyr)
library(sqldf)
library(xlsx)
library(stringr)




##  LOAD THE DATA
raw_data <- read.csv(file = "/Users/User/Dropbox/transport and conflict/Stones survey paper/Data/CSV/Sheet_1.csv", header = TRUE, na.strings = "")


##  CURRENT VARIABLE NAMES ARE SO LONG.  RENAME SO THEY'RE EASIER TO WORK WITH.
raw_data <- rename(x = raw_data, 
                   replace = c( "In.what.SSA.countries.do.you.have.professional.experience....Open.Ended.Response" = "countries.experience", 
                                "Which.of.the.following.best.describes.your.professional.affiliation...please.check.only.one." = "affiliaiton",
                                "Which.of.the.following.best.describes.your.professional.affiliation...please.check.only.one....Other..please.specify." = "affiliation_other",
                                "Which.of.the.following.best.describes.your.professional.specialty...please.check.only.one." = "specialty",
                                "Which.of.the.following.best.describes.your.professional.specialty...please.check.only.one....Other..please.specify." = "specialty_other",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Vulnerable.road.users..pedestrians..bicyclists..pushcarts..animal.carts..etc.." = "vulnerable.users.most.dangerous",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Transport.services..buses.and.minibuses." = "buses.most.dangerous",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Motorcycles..including.motorcycle.taxis." = "motorcycles.most.dangerous",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Comments." = "most.dangerous.comments",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Condition.of.travelway..e.g...potholes.and.ruts.that.can.cause.vehicles.to.lose.control.or.encourage.drivers.to.leave.their.travel.lane.to.avoid.hitting" = "need.research.travelway.condition",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Lack.of.adequate.signage..e.g...warning.signs.for.horizontal.curves.or.other.approaching.hazards." = "need.research.signage",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Dust.from.unpaved.roads.obscuring.visibility.or.resulting.in.risky.driving.behavior..e.g...overtaking.vehicles.to.avoid.following.along.in.their.dust." = "need.research.dust",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Other..please.specify." = "need.research.other",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Aggressive.driving..speeding..overtaking." = "need.research.aggressive.driving",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Driving.under.the.influence..alcohol..drugs." = "need.research.DUI",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Distracted.driving..texting..talking.on.mobile.phones." = "need.research.distracted.driving",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Non.use.of.safety.equipment..seatbelts..helmets..child.restraints." = "need.research.nonuse.safety.equipment",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Poor.driving..education.experience..licensing.regulation." = "need.research.education.regulation",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Comments." = "need.research.comments",
                                "To.what.extent.do.you.agree.with.the.following.statements....Rural.road.crashes.disproportionately.impact.women" = "crashes.disproportionately.impact.women",
                                "To.what.extent.do.you.agree.with.the.following.statements....Rural.road.crashes.disproportionately.impact.children" = "crashes.disproportionately.impact.kids",
                                "Which.of.the.following.do.you.consider.to.be.the.most.pressing.accessibility.issue.in.rural.Africa...." = "most.pressing.accessibility",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.maternal.pre.natal.care" = "access.maternal.care",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.general.preventitive.care" = "access.general.care",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.pediatric.care" = "access.pediatric.care",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.disease.chronic.condition.care" = "access.chronic.care",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.emergency.trauma.care" = "access.trauma.care",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Comments." = "access.comments",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Safety.education.awareness.for.children.and.parents" = "research.safety.education",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Provision.of.transport.servcies.for.schoolchildren" = "research.school.transport",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Provision.of.safer.pedestrian.facilities" = "research.school.pedestrian",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Comments." = "research.education.comments",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Transport.of.agricultural.goods.to.market" = "access.goods2market",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Connectivity.to.service.sector.employment.in.nearby.populated.areas" = "access.labor.market",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Natural.resource.extraction" = "access.natural.resources",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Comments." = "econ.access.comments",
                                "To.what.extent.do.you.agree.with.the.following.statement...br..br..em.Dust.and.other.air.pollution.from.rural.roads.in.Africa.have.not.received.adequate.attention.from.the.research.and.policy.communities...em...." = "inadequate.attention.dust",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Individuals.in.the.immediate.roadway.environment..pedestrians..cyclists." = "air.pollution.vulnerable.users",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Occupants.of.residences.and.business.operations.close.to.the.roadway....300m." = "air.pollution.local.residents.businesses",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Vehicle.passengers..cars..buses..minibuses..motorcyclists." = "air.pollution.vehicle.passengers",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Comments." = "air.pollution.comments",
                                "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Building.rural.roads" = "PCS.building.roads",
                                "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Provision.of.transport.services" = "PCS.transport.provision",
                                "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Comments." = "PCS.comments"))



##  SOME RESPONDENTS MAY HAVE TAKEN THE SURVEY MULTIPLE TIMES. 
##  DROP THE REPEATED OBSERVATIONS.

  # Identify the surveys that share an IP address
  temp <- rbind( subset(x=raw_data, subset=duplicated(raw_data$IP.Address, fromLast=TRUE)),
                 subset(x=raw_data, subset=duplicated(raw_data$IP.Address, fromLast=FALSE)) )
  #View( temp[ order(temp$IP.Address, temp$StartDate), ] )

  # There are three sets of responses that originate from the same IP address
  # at the exact same second, but one of the responses includes a value for one of
  # the questions (healthcare accessibility, maternal and prenatal care) while the 
  # other does not.  Drop the observation that is missing a value for that question.
  raw_data <- subset(x=raw_data, 
                     subset= !duplicated(cbind(raw_data$StartDate, raw_data$IP.Address)) |
                             !is.na(raw_data$access.maternal.care))






## ADD VARIABLES

  # Use IP address to identify respondent's current continent and country
  URLs <- paste0('http://www.ipgeek.net/',
                 as.character(raw_data$IP.Address))

  for(i in 1:length(URLs)){
    
    # Query IP location page
    webpages <- getURL(URLs[i])

    # Extract country info
    temp1 <- gregexpr("<th>Country:</th><td>", webpages)[[1]]
    temp2 <- gregexpr(" <img src=\"/flag", webpages)[[1]]
    raw_data$access.from.country[i] <- substring(webpages, temp1[1] + attr(temp1, which = "match.length"), temp2[1])
      raw_data$access.from.country[i] <- str_trim(raw_data$access.from.country[i])

    # Extract continent info
    temp1 <- gregexpr("<th>Continent:</th><td>", webpages)[[1]]
    temp2 <- gregexpr("</td></tr>\r\n<tr><th colspan=2>", webpages)[[1]]
    raw_data$access.from.continent[i] <- substring(webpages, temp1[1] + attr(temp1, which = "match.length"), temp2[1] - 1)
      raw_data$access.from.continent[i] <- str_trim(raw_data$access.from.continent[i])
    
    Sys.sleep(3)
    
  }

  # Website does not report continent for the Americas
  raw_data$access.from.continent[ raw_data$access.from.continent == 'NA' ] <- 'AM'


  # Calculate time that respondent spent on the survey

  raw_data$StartDate <- as.POSIXct(x = strptime(x = raw_data$StartDate, format = "%m/%d/%Y %T"))
  raw_data$EndDate <- as.POSIXct(x = strptime(x = raw_data$EndDate, format = "%m/%d/%Y %T"))
  raw_data$survey.length.of.time <- (raw_data$EndDate - raw_data$StartDate)


#   # Countries that respondents have experience in
#   
#     # First identify SSA countries
#     SSA.Wikipedia <- getURL(url = 'http://en.wikipedia.org/wiki/Sub-Saharan_Africa')
#     SSA.countries <- as.character( readHTMLTable(doc=SSA.Wikipedia)[[1]]$Country )
# 
#     # Create a matrix for dummy coding
#     SSA.matrix <- matrix(data=0, 
#                          nrow=nrow(data),
#                          ncol=length(SSA.countries))
#       colnames(SSA.matrix) <- sort(SSA.countries)
# 
#     # Cycle through each respondent and country, marking the countries that the respondent has professional experience in
#     for(i in 1:nrow(SSA.matrix)){     # respondent
#       for(j in 1:ncol(SSA.matrix)){   # country
# 
#         temp <- pmatch(x = colnames(SSA.matrix)[j], 
#                        table = as.character( data$In.what.SSA.countries.do.you.have.professional.experience....Open.Ended.Response[i] ))
#         if( !is.na(temp) )  SSA.matrix[i,j] <- 1
#         
#       }
#     }
# 
#     sum(SSA.matrix)


  # Some variables should be combined into a single rank variable
  #names(raw_data)



## PREPROCESSING

# Grab respondent variables
respondent_data <- sqldf("SELECT  affiliaiton,
                                  specialty,
                                  access_from_continent,
                                  access_from_country,
                                  StartDate,
                                  survey_length_of_time
                          FROM    raw_data")


# Grab two-level variables
two_level_data <- sqldf("SELECT Interested_in_answering_one_last_question_
                         FROM   raw_data")

# Grab three-level variables
three_level_data <- sqldf("SELECT   vulnerable_users_most_dangerous,
                                    buses_most_dangerous,
                                    motorcycles_most_dangerous,
                                    most_pressing_accessibility,
                                    inadequate_attention_dust,
                                    air_pollution_vulnerable_users,
                                    air_pollution_local_residents_businesses,
                                    air_pollution_vehicle_passengers
                            FROM    raw_data")

  # Change variable types where necessary
  three_level_data$vulnerable_users_most_dangerous <- ordered(three_level_data$vulnerable_users_most_dangerous,
                                                              levels = c("Least dangerous", "Dangerous", "Most dangerous"))

  three_level_data$buses_most_dangerous <- ordered(three_level_data$buses_most_dangerous,
                                                   levels = c("Least dangerous", "Dangerous", "Most dangerous"))

  three_level_data$motorcycles_most_dangerous <- ordered(three_level_data$motorcycles_most_dangerous,
                                                         levels = c("Least dangerous", "Dangerous", "Most dangerous"))

  three_level_data$air_pollution_vulnerable_users <- ordered(three_level_data$air_pollution_vulnerable_users,
                                                             levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))

  three_level_data$air_pollution_local_residents_businesses <- ordered(three_level_data$air_pollution_local_residents_businesses,
                                                                       levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))

  three_level_data$air_pollution_vehicle_passengers <- ordered(three_level_data$air_pollution_vehicle_passengers,
                                                               levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))


# Grab five-level variables
five_level_data <- sqldf("SELECT  need_research_travelway_condition,
                                  need_research_signage,
                                  need_research_dust,
                                  need_research_aggressive_driving,
                                  need_research_DUI,
                                  need_research_distracted_driving,
                                  need_research_nonuse_safety_equipment,
                                  need_research_education_regulation,
                                  research_safety_education,
                                  research_school_pedestrian,
                                  research_school_transport,
                                  crashes_disproportionately_impact_women,
                                  crashes_disproportionately_impact_kids,
                                  access_goods2market,
                                  access_labor_market,
                                  access_natural_resources,
                                  access_maternal_care,
                                  access_general_care,
                                  access_pediatric_care,
                                  access_chronic_care,
                                  access_trauma_care,
                                  PCS_building_roads,
                                  PCS_transport_provision
                          FROM    raw_data")


  five_level_data <- apply(X=five_level_data, 
                           MARGIN=2, 
                           FUN=function(x) replace(x = x, 
                                                   list = x == "Very unimportant - has already been adequately addressed or is not a major issue", 
                                                   values = "Very unimportant"))

  five_level_data <- apply(X=five_level_data, 
                           MARGIN=2, 
                           FUN=function(x) replace(x = x, 
                                                   list = x == "Unimportant - warrants continued study as opportunities arise", 
                                                   values = "Unimportant"))

  five_level_data <- apply(X=five_level_data, 
                           MARGIN=2, 
                           FUN=function(x) replace(x = x, 
                                                   list = x == "Moderately important - should be included in future research plans", 
                                                   values = "Moderately important"))

  five_level_data <- apply(X=five_level_data, 
                           MARGIN=2, 
                           FUN=function(x) replace(x = x, 
                                                   list = x == "Important - should be addressed within the next 5 years", 
                                                   values = "Important"))

  five_level_data <- apply(X=five_level_data, 
                           MARGIN=2, 
                           FUN=function(x) replace(x = x, 
                                                   list = x == "Very important - requires immediate attention", 
                                                   values = "Very important"))



  # Save as a data frame
  five_level_data <- data.frame(five_level_data)



  # Identify missing values, change variable types where necessary
  five_level_data$need_research_travelway_condition <- ordered(five_level_data$need_research_travelway_condition,
                                                               levels = c("Very unimportant",
                                                                          "Unimportant",
                                                                          "Moderately important",
                                                                          "Important",
                                                                          "Very important"))

  five_level_data$need_research_dust <- ordered(five_level_data$need_research_dust,
                                                levels = c("Very unimportant",
                                                           "Unimportant",
                                                           "Moderately important",
                                                           "Important",
                                                           "Very important"))

  five_level_data$need_research_aggressive_driving <- ordered(five_level_data$need_research_aggressive_driving,
                                                              levels = c("Very unimportant",
                                                                         "Unimportant",
                                                                         "Moderately important",
                                                                         "Important",
                                                                         "Very important"))

  five_level_data$need_research_DUI <- ordered(five_level_data$need_research_DUI,
                                               levels = c("Very unimportant",
                                                          "Unimportant",
                                                          "Moderately important",
                                                          "Important",
                                                          "Very important"))

  five_level_data$need_research_distracted_driving <- ordered(five_level_data$need_research_distracted_driving,
                                                              levels = c("Very unimportant",
                                                                         "Unimportant",
                                                                         "Moderately important",
                                                                         "Important",
                                                                         "Very important"))

  five_level_data$need_research_nonuse_safety_equipment <- ordered(five_level_data$need_research_nonuse_safety_equipment,
                                                                   levels = c("Very unimportant",
                                                                              "Unimportant",
                                                                              "Moderately important",
                                                                              "Important",
                                                                              "Very important"))

  five_level_data$need_research_education_regulation <- ordered(five_level_data$need_research_education_regulation,
                                                                levels = c("Very unimportant",
                                                                           "Unimportant",
                                                                           "Moderately important",
                                                                           "Important",
                                                                           "Very important"))

  five_level_data$research_safety_education <- ordered(five_level_data$research_safety_education,
                                                       levels = c("Very unimportant",
                                                                  "Unimportant",
                                                                  "Moderately important",
                                                                  "Important",
                                                                  "Very important"))

  five_level_data$research_school_pedestrian <- ordered(five_level_data$research_school_pedestrian,
                                                        levels = c("Very unimportant",
                                                                   "Unimportant",
                                                                   "Moderately important",
                                                                   "Important",
                                                                   "Very important"))

  five_level_data$research_school_transport <- ordered(five_level_data$research_school_transport,
                                                       levels = c("Very unimportant",
                                                                  "Unimportant",
                                                                  "Moderately important",
                                                                  "Important",
                                                                  "Very important"))

  five_level_data$crashes_disproportionately_impact_women <- ordered(five_level_data$crashes_disproportionately_impact_women,
                                                                     levels = c("Strongly Disagree",
                                                                                "Disagree",
                                                                                "Neither Disagree Nor Agree",
                                                                                "Agree",
                                                                                "Strongly Agree"))

  five_level_data$crashes_disproportionately_impact_kids <- ordered(five_level_data$crashes_disproportionately_impact_kids,
                                                                    levels = c("Strongly Disagree",
                                                                               "Disagree",
                                                                               "Neither Disagree Nor Agree",
                                                                               "Agree",
                                                                               "Strongly Agree"))

  five_level_data$access_goods2market <- ordered(five_level_data$access_goods2market,
                                                 levels = c("No relationship",
                                                            "Indirect relationship",
                                                            "Direct relationship",
                                                            "Strong relationship"))

  five_level_data$access_labor_market <- ordered(five_level_data$access_labor_market,
                                                 levels = c("No relationship",
                                                            "Indirect relationship",
                                                            "Direct relationship",
                                                            "Strong relationship"))

  five_level_data$access_natural_resources <- ordered(five_level_data$access_natural_resources,
                                                      levels = c("No relationship",
                                                                 "Indirect relationship",
                                                                 "Direct relationship",
                                                                 "Strong relationship"))

  five_level_data$access_maternal_care <- ordered(five_level_data$access_maternal_care,
                                                  levels = c(" 1", " 2", " 3", " 4", " 5"))

  five_level_data$access_general_care <- ordered(five_level_data$access_general_care,
                                                 levels = c(" 1", " 2", " 3", " 4", " 5"))

  five_level_data$access_pediatric_care <- ordered(five_level_data$access_pediatric_care,
                                                   levels = c(" 1", " 2", " 3", " 4", " 5"))

  five_level_data$access_chronic_care <- ordered(five_level_data$access_chronic_care,
                                                 levels = c(" 1", " 2", " 3", " 4", " 5"))

  five_level_data$access_trauma_care <- ordered(five_level_data$access_trauma_care,
                                                levels = c(" 1", " 2", " 3", " 4", " 5"))

  five_level_data$PCS_building_roads <- ordered(five_level_data$PCS_building_roads, 
                                                levels = c(levels(five_level_data$PCS_building_roads)[4], 
                                                           levels(five_level_data$PCS_building_roads)[2], 
                                                           levels(five_level_data$PCS_building_roads)[3], 
                                                           levels(five_level_data$PCS_building_roads)[1], 
                                                           levels(five_level_data$PCS_building_roads)[5]))

  five_level_data$PCS_transport_provision <- ordered(five_level_data$PCS_transport_provision, 
                                                     levels = c(levels(five_level_data$PCS_transport_provision)[4], 
                                                                levels(five_level_data$PCS_transport_provision)[2], 
                                                                levels(five_level_data$PCS_transport_provision)[3], 
                                                                levels(five_level_data$PCS_transport_provision)[1], 
                                                                levels(five_level_data$PCS_transport_provision)[5]))

# Combine into single dataframe

data <- data.frame(respondent_data, 
                   two_level_data,
                   three_level_data,
                   five_level_data)


## SAVE DATA FOR ANALYSIS
## Use RData format to preserve ordered variables (CSV does not retain that information)
save(data, '/Users/User/Dropbox/transport and conflict/Stones survey paper/Data/analysis.RData', ascii = TRUE)

