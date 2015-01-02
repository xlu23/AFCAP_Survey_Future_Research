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
                   replace = c( "StartDate" = "Start Date",
                                "EndDate" = "End Date",
                                "In.what.SSA.countries.do.you.have.professional.experience....Open.Ended.Response" = "Experience in Countries", 
                                "Which.of.the.following.best.describes.your.professional.affiliation...please.check.only.one." = "Professional Affiliation",
                                "Which.of.the.following.best.describes.your.professional.affiliation...please.check.only.one....Other..please.specify." = "Other Affiliation",
                                "Which.of.the.following.best.describes.your.professional.specialty...please.check.only.one." = "Professional Specialty",
                                "Which.of.the.following.best.describes.your.professional.specialty...please.check.only.one....Other..please.specify." = "Other Specialty",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Vulnerable.road.users..pedestrians..bicyclists..pushcarts..animal.carts..etc.." = "Danger: Vulnerable Road Users",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Transport.services..buses.and.minibuses." = "Danger: Buses",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Motorcycles..including.motorcycle.taxis." = "Danger: Motorcycles",
                                "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Comments." = "Danger: Comments",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Condition.of.travelway..e.g...potholes.and.ruts.that.can.cause.vehicles.to.lose.control.or.encourage.drivers.to.leave.their.travel.lane.to.avoid.hitting" = "Research Need: Travelway Condition",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Lack.of.adequate.signage..e.g...warning.signs.for.horizontal.curves.or.other.approaching.hazards." = "Research Need: Signage",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Dust.from.unpaved.roads.obscuring.visibility.or.resulting.in.risky.driving.behavior..e.g...overtaking.vehicles.to.avoid.following.along.in.their.dust." = "Research Need: Dust",
                                "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Other..please.specify." = "Research Need: Other",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Aggressive.driving..speeding..overtaking." = "Research Need: Aggressive Driving",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Driving.under.the.influence..alcohol..drugs." = "Research Need: DUI",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Distracted.driving..texting..talking.on.mobile.phones." = "Research Need: Distracted Driving",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Non.use.of.safety.equipment..seatbelts..helmets..child.restraints." = "Research Need: Non-Use of Safety Equipment",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Poor.driving..education.experience..licensing.regulation." = "Research Need: Education & Regulation",
                                "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Comments." = "Research Need: Comments",
                                "To.what.extent.do.you.agree.with.the.following.statements....Rural.road.crashes.disproportionately.impact.women" = "Crashes Disproportionately Impact Women",
                                "To.what.extent.do.you.agree.with.the.following.statements....Rural.road.crashes.disproportionately.impact.children" = "Crashes Disproportionately Impact Children",
                                "Which.of.the.following.do.you.consider.to.be.the.most.pressing.accessibility.issue.in.rural.Africa...." = "Most Pressing Accessibility Issue",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.maternal.pre.natal.care" = "Healthcare Access: Maternal",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.general.preventitive.care" = "Healthcare Access: General",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.pediatric.care" = "Healthcare Access: Pediatric",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.disease.chronic.condition.care" = "Healthcare Access: Chronic",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.emergency.trauma.care" = "Healthcare Access: Trauma",
                                "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Comments." = "Healthcare Access: Comments",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Safety.education.awareness.for.children.and.parents" = "Education Access: Safety Education",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Provision.of.transport.servcies.for.schoolchildren" = "Education Access: School Transport",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Provision.of.safer.pedestrian.facilities" = "Education Access: Pedestrian Facilities",
                                "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Comments." = "Education Access: Comments",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Transport.of.agricultural.goods.to.market" = "Economic Access: Goods to Market",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Connectivity.to.service.sector.employment.in.nearby.populated.areas" = "Economic Access: Connectivity to Employment",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Natural.resource.extraction" = "Economic Access: Natural-Resource Extraction",
                                "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Comments." = "Economic Access: Comments",
                                "To.what.extent.do.you.agree.with.the.following.statement...br..br..em.Dust.and.other.air.pollution.from.rural.roads.in.Africa.have.not.received.adequate.attention.from.the.research.and.policy.communities...em...." = "Inadequate Attention: Dust",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Individuals.in.the.immediate.roadway.environment..pedestrians..cyclists." = "Air Pollution: Vulnerable Users",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Occupants.of.residences.and.business.operations.close.to.the.roadway....300m." = "Air Pollution: Local Residents/Businesses",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Vehicle.passengers..cars..buses..minibuses..motorcyclists." = "Air Pollution: Vehicle Passengers",
                                "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Comments." = "Air Pollution: Comments",
                                "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Building.rural.roads" = "Peace/Stability: Building Roads",
                                "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Provision.of.transport.services" = "Peace/Stability: Transport Provision",
                                "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Comments." = "Peace/Stability: Comments"))



##  SOME RESPONDENTS MAY HAVE TAKEN THE SURVEY MULTIPLE TIMES. 
##  DROP THE REPEATED OBSERVATIONS.

  # Identify the surveys that share an IP address
  temp <- rbind( subset(x=raw_data, subset=duplicated(raw_data$IP.Address, fromLast=TRUE)),
                 subset(x=raw_data, subset=duplicated(raw_data$IP.Address, fromLast=FALSE)) )
  #View( temp[ order(temp$IP.Address, temp$"Start Date"), ] )

  # There are three sets of responses that originate from the same IP address
  # at the exact same second, but one of the responses includes a value for one of
  # the questions (healthcare accessibility, maternal and prenatal care) while the 
  # other does not.  Drop the observation that is missing a value for that question.
  raw_data <- subset(x=raw_data, 
                     subset= !duplicated(cbind(raw_data$"Start Date", raw_data$IP.Address)) |
                             !is.na(raw_data$"Healthcare Access: Maternal"))






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
    raw_data$"Access from Country"[i] <- substring(webpages, temp1[1] + attr(temp1, which = "match.length"), temp2[1])
      raw_data$"Access from Country"[i] <- str_trim(raw_data$"Access from Country"[i])

    # Extract continent info
    temp1 <- gregexpr("<th>Continent:</th><td>", webpages)[[1]]
    temp2 <- gregexpr("</td></tr>\r\n<tr><th colspan=2>", webpages)[[1]]
    raw_data$"Access from Continent"[i] <- substring(webpages, temp1[1] + attr(temp1, which = "match.length"), temp2[1] - 1)
      raw_data$"Access from Continent"[i] <- str_trim(raw_data$"Access from Continent"[i])
    
    Sys.sleep(3)
    
  }

  # Website does not report continent for the Americas
  raw_data$"Access from Continent"[ raw_data$"Access from Continent" == 'NA' ] <- 'AM'


  # Calculate time that respondent spent on the survey

  raw_data$StartDate <- as.POSIXct(x = strptime(x = raw_data$StartDate, format = "%m/%d/%Y %T"))
  raw_data$EndDate <- as.POSIXct(x = strptime(x = raw_data$EndDate, format = "%m/%d/%Y %T"))
  raw_data$"Survey Length of Time" <- (raw_data$EndDate - raw_data$StartDate)



  # Some variables should be combined into a single rank variable
  #names(raw_data)



## PREPROCESSING
names(raw_data)
# Grab respondent variables
respondent_data <- sqldf("SELECT  'Professional Affiliation',
                                  'Professional Specialty',
                                  'Access from Continent',
                                  'Access from Country',
                                  StartDate,
                                  'Survey Length of Time'
                          FROM    raw_data")


# Grab two-level variables
two_level_data <- sqldf("SELECT Interested_in_answering_one_last_question_
                         FROM   raw_data")

# Grab three-level variables
three_level_data <- sqldf("SELECT   'Danger: Vulnerable Road Users',
                                    'Danger: Buses',
                                    'Danger: Motorcycles',
                                    'Most Pressing Accessibility Issue',
                                    'Inadequate Attention: Dust',
                                    'Air Pollution: Vulnerable Users',
                                    'Air Pollution: Local Residents/Businesses',
                                    'Air Pollution: Vehicle Passengers'
                            FROM    raw_data")

  # Change variable types where necessary
  three_level_data$vulnerable_users_most_dangerous <- ordered(three_level_data$"Danger: Vulnerable Road Users",
                                                              levels = c("Least dangerous", "Dangerous", "Most dangerous"))

  three_level_data$buses_most_dangerous <- ordered(three_level_data$"Danger: Buses",
                                                   levels = c("Least dangerous", "Dangerous", "Most dangerous"))

  three_level_data$motorcycles_most_dangerous <- ordered(three_level_data$"Danger: Motorcycles",
                                                         levels = c("Least dangerous", "Dangerous", "Most dangerous"))

  three_level_data$air_pollution_vulnerable_users <- ordered(three_level_data$"Air Pollution: Vulnerable Users",
                                                             levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))

  three_level_data$air_pollution_local_residents_businesses <- ordered(three_level_data$"Air Pollution: Local Residents/Businesses",
                                                                       levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))

  three_level_data$air_pollution_vehicle_passengers <- ordered(three_level_data$"Air Pollution: Vehicle Passengers",
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
save(data, file = '/Users/User/Dropbox/transport and conflict/Stones survey paper/Data/analysis.RData', ascii = TRUE)

