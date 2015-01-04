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





# LOAD LIBRARIES ----------------------------------------------------------

  library(RCurl)
  library(XML)
  library(plyr)
  library(sqldf)
  library(xlsx)
  library(stringr)






# LOAD THE DATA -----------------------------------------------------------

  raw_data <- read.csv(file = "/Users/User/Dropbox/transport and conflict/Stones survey paper/Data/CSV/Sheet_1.csv", header = TRUE, na.strings = "")







# SHORTEN VARIABLE NAMES --------------------------------------------------

  raw_data <- rename(x = raw_data, 
                     replace = c( "In.what.SSA.countries.do.you.have.professional.experience....Open.Ended.Response" = "countries_experience", 
                                  "Which.of.the.following.best.describes.your.professional.affiliation...please.check.only.one." = "affiliation",
                                  "Which.of.the.following.best.describes.your.professional.affiliation...please.check.only.one....Other..please.specify." = "affiliation_other",
                                  "Which.of.the.following.best.describes.your.professional.specialty...please.check.only.one." = "specialty",
                                  "Which.of.the.following.best.describes.your.professional.specialty...please.check.only.one....Other..please.specify." = "specialty_other",
                                  "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Vulnerable.road.users..pedestrians..bicyclists..pushcarts..animal.carts..etc.." = "vulnerable_road_users_most_dangerous",
                                  "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Transport.services..buses.and.minibuses." = "transport_services_most_dangerous",
                                  "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Motorcycles..including.motorcycle.taxis." = "motorcycles_most_dangerous",
                                  "In.your.opinion..which.of.the.following.transport.modes.is.the.most.dangerous.in.rural.Africa....Comments." = "most_dangerous_comments",
                                  "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Condition.of.travelway..e.g...potholes.and.ruts.that.can.cause.vehicles.to.lose.control.or.encourage.drivers.to.leave.their.travel.lane.to.avoid.hitting" = "need_research_condition_of_travelway",
                                  "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Lack.of.adequate.signage..e.g...warning.signs.for.horizontal.curves.or.other.approaching.hazards." = "need_research_lack_of_adequate_signage",
                                  "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Dust.from.unpaved.roads.obscuring.visibility.or.resulting.in.risky.driving.behavior..e.g...overtaking.vehicles.to.avoid.following.along.in.their.dust." = "need_research_dust_from_unpaved_roads",
                                  "To.the.extent.that.road.conditions.contribute.to.rural.road.crashes.in.Africa..which.of.the.following.issues.need..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Other..please.specify." = "need_research_other",
                                  "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Aggressive.driving..speeding..overtaking." = "need_research_aggressive_driving",
                                  "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Driving.under.the.influence..alcohol..drugs." = "need_research_DUI",
                                  "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Distracted.driving..texting..talking.on.mobile.phones." = "need_research_distracted_driving",
                                  "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Non.use.of.safety.equipment..seatbelts..helmets..child.restraints." = "need_research_nonuse_safety_equipment",
                                  "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Poor.driving..education.experience..licensing.regulation." = "need_research_poor_driving",
                                  "To.the.extent.that.risky.driving.behaviors.contribute.to.rural.road.crashes..which.of.the.following.potential.causal.factors.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Comments." = "need_research_comments",
                                  "To.what.extent.do.you.agree.with.the.following.statements....Rural.road.crashes.disproportionately.impact.women" = "crashes_disproportionately_impact_women",
                                  "To.what.extent.do.you.agree.with.the.following.statements....Rural.road.crashes.disproportionately.impact.children" = "crashes_disproportionately_impact_kids",
                                  "Which.of.the.following.do.you.consider.to.be.the.most.pressing.accessibility.issue.in.rural.Africa...." = "most_pressing_accessibility_issue",
                                  "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.maternal.pre.natal.care" = "health_access_maternal_prenatal",
                                  "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.general.preventitive.care" = "health_access_general_preventative",
                                  "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.pediatric.care" = "health_access_pediatric",
                                  "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.disease.chronic.condition.care" = "health_access_disease_chronic_condition",
                                  "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Access.to.emergency.trauma.care" = "health_access_emergency_trauma",
                                  "With.regard.to.the.role.transport.plays.in.healthcare.accessibility.in.rural.Africa..please.rank.the.following.issues.according.to.need.for..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention..5...highest.need.....Comments." = "health_access_comments",
                                  "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Safety.education.awareness.for.children.and.parents" = "education_need_research_safety_education_awareness",
                                  "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Provision.of.transport.servcies.for.schoolchildren" = "education_need_research_provision_of_transport_services",
                                  "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Provision.of.safer.pedestrian.facilities" = "education_need_provision_of_pedestrian_facilities",
                                  "With.regard.to.the.role.transport.plays.in.education.accessibility.in.rural.Africa..which.of.the.following.issues.need.immediate..span.style..text.decoration..underline....em..strong.research..strong...em...span..attention....Comments." = "education_need_research_comments",
                                  "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Transport.of.agricultural.goods.to.market" = "economic_access_agriculture2market",
                                  "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Connectivity.to.service.sector.employment.in.nearby.populated.areas" = "economic_access_labor2service_sector",
                                  "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Natural.resource.extraction" = "economic_access_natural_resource_extraction",
                                  "With.regard.to.the.role.transport.plays.in.economic.accessibility.in.rural.Africa..which.of.the.following.issues.most.affect..or.are.affected.by..road.safety....Comments." = "economic_access_comments",
                                  "To.what.extent.do.you.agree.with.the.following.statement...br..br..em.Dust.and.other.air.pollution.from.rural.roads.in.Africa.have.not.received.adequate.attention.from.the.research.and.policy.communities...em...." = "inadequate_attention_dust",
                                  "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Individuals.in.the.immediate.roadway.environment..pedestrians..cyclists." = "air_pollution_individuals_in_roadway_environment",
                                  "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Occupants.of.residences.and.business.operations.close.to.the.roadway....300m." = "air_pollution_roadside_residents_businesses",
                                  "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Vehicle.passengers..cars..buses..minibuses..motorcyclists." = "air_pollution_vehicle_passengers",
                                  "In.your.opinion..which.of.the.following.groups.are.most.at.risk.due.to.air.pollution.in..em..span.style..text.decoration..underline....strong.rural..strong...span...em..Africa....Comments." = "air_pollution_comments",
                                  "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Building.rural.roads" = "PCS_building_roads",
                                  "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Provision.of.transport.services" = "PCS_transport_provision",
                                  "To.what.extent.do.you.think.the.following.play.a.role.in.promoting.stability.in.fragile.and.conflict.affected.regions.in.rural.Africa....Comments." = "PCS_comments"))






# DROP RESPONSES FROM SAME RESPONDENT -------------------------------------

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
                       !is.na(raw_data$health_access_maternal_prenatal))








# ADD VARIABLES -----------------------------------------------------------
  
  # Use IP address to identify respondent's current continent and country
  URLs <- paste0('http://www.ipgeek.net/',
                 as.character(raw_data$IP.Address))
  
  for(i in 1:length(URLs)){
    
    # Query IP location page
    webpages <- getURL(URLs[i])
    
    # Extract country info
    temp1 <- gregexpr("<th>Country:</th><td>", webpages)[[1]]
    temp2 <- gregexpr(" <img src=\"/flag", webpages)[[1]]
    raw_data$access_from_country[i] <- substring(webpages, temp1[1] + attr(temp1, which = "match.length"), temp2[1])
    raw_data$access_from_country[i] <- str_trim(raw_data$access_from_country[i])
    
    # Extract continent info
    temp1 <- gregexpr("<th>Continent:</th><td>", webpages)[[1]]
    temp2 <- gregexpr("</td></tr>\r\n<tr><th colspan=2>", webpages)[[1]]
    raw_data$access_from_continent[i] <- substring(webpages, temp1[1] + attr(temp1, which = "match.length"), temp2[1] - 1)
    raw_data$access_from_continent[i] <- str_trim(raw_data$access_from_continent[i])
    
    Sys.sleep(3)
    
  }
  
  # Website does not report continent for the Americas
  raw_data$access_from_continent[ raw_data$access_from_continent == 'NA' ] <- 'AM'
  
  
  # Calculate time that respondent spent on the survey
  
  raw_data$StartDate <- as.POSIXct(x = strptime(x = raw_data$StartDate, format = "%m/%d/%Y %T"))
  raw_data$EndDate <- as.POSIXct(x = strptime(x = raw_data$EndDate, format = "%m/%d/%Y %T"))
  raw_data$survey_length_of_time <- as.numeric(raw_data$EndDate - raw_data$StartDate)









# SIMPLIFY AND ORDER RESPONSES --------------------------------------------


  # Grab respondent variables
  respondent_data <- sqldf("SELECT  affiliation,
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
  three_level_data <- sqldf("SELECT   vulnerable_road_users_most_dangerous,
                                      transport_services_most_dangerous,
                                      motorcycles_most_dangerous,
                                      most_pressing_accessibility_issue,
                                      inadequate_attention_dust,
                                      air_pollution_individuals_in_roadway_environment,
                                      air_pollution_roadside_residents_businesses,
                                      air_pollution_vehicle_passengers
                              FROM    raw_data")
  
  # Change variable types where necessary
  three_level_data$vulnerable_road_users_most_dangerous <- ordered(three_level_data$vulnerable_road_users_most_dangerous,
                                                                   levels = c("Least dangerous", "Dangerous", "Most dangerous"))
  
  three_level_data$transport_services_most_dangerous <- ordered(three_level_data$transport_services_most_dangerous,
                                                                levels = c("Least dangerous", "Dangerous", "Most dangerous"))
  
  three_level_data$motorcycles_most_dangerous <- ordered(three_level_data$motorcycles_most_dangerous,
                                                         levels = c("Least dangerous", "Dangerous", "Most dangerous"))
  
  three_level_data$air_pollution_individuals_in_roadway_environment <- ordered(three_level_data$air_pollution_individuals_in_roadway_environment,
                                                                               levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))
  
  three_level_data$air_pollution_roadside_residents_businesses <- ordered(three_level_data$air_pollution_roadside_residents_businesses,
                                                                          levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))
  
  three_level_data$air_pollution_vehicle_passengers <- ordered(three_level_data$air_pollution_vehicle_passengers,
                                                               levels = c("Least vulnerable", "Vulnerable", "Most vulnerable"))
  
  
  # Grab five-level variables
  five_level_data <- sqldf("SELECT  need_research_condition_of_travelway,
                                    need_research_lack_of_adequate_signage,
                                    need_research_dust_from_unpaved_roads,
                                    need_research_aggressive_driving,
                                    need_research_DUI,
                                    need_research_distracted_driving,
                                    need_research_nonuse_safety_equipment,
                                    need_research_poor_driving,
                                    education_need_research_safety_education_awareness,
                                    education_need_provision_of_pedestrian_facilities,
                                    education_need_research_provision_of_transport_services,
                                    crashes_disproportionately_impact_women,
                                    crashes_disproportionately_impact_kids,
                                    economic_access_agriculture2market,
                                    economic_access_labor2service_sector,
                                    economic_access_natural_resource_extraction,
                                    health_access_maternal_prenatal,
                                    health_access_general_preventative,
                                    health_access_pediatric,
                                    health_access_disease_chronic_condition,
                                    health_access_emergency_trauma,
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
  five_level_data$need_research_condition_of_travelway <- ordered(five_level_data$need_research_condition_of_travelway,
                                                                  levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))
  
  five_level_data$need_research_dust_from_unpaved_roads <- ordered(five_level_data$need_research_dust_from_unpaved_roads,
                                                                   levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$need_research_aggressive_driving <- ordered(five_level_data$need_research_aggressive_driving,
                                                              levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$need_research_DUI <- ordered(five_level_data$need_research_DUI,
                                               levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$need_research_distracted_driving <- ordered(five_level_data$need_research_distracted_driving,
                                                              levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$need_research_nonuse_safety_equipment <- ordered(five_level_data$need_research_nonuse_safety_equipment,
                                                                   levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$need_research_poor_driving <- ordered(five_level_data$need_research_poor_driving,
                                                        levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$education_need_research_safety_education_awareness <- ordered(five_level_data$education_need_research_safety_education_awareness,
                                                                                levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$education_need_provision_of_pedestrian_facilities <- ordered(five_level_data$education_need_provision_of_pedestrian_facilities,
                                                                               levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$education_need_research_provision_of_transport_services <- ordered(five_level_data$education_need_research_provision_of_transport_services,
                                                                                     levels = c("Very unimportant", "Unimportant", "Moderately important", "Important", "Very important"))

  five_level_data$crashes_disproportionately_impact_women <- ordered(five_level_data$crashes_disproportionately_impact_women,
                                                                     levels = c("Strongly Disagree", "Disagree", "Neither Disagree Nor Agree", "Agree", "Strongly Agree"))
  
  five_level_data$crashes_disproportionately_impact_kids <- ordered(five_level_data$crashes_disproportionately_impact_kids,
                                                                    levels = c("Strongly Disagree", "Disagree", "Neither Disagree Nor Agree", "Agree", "Strongly Agree"))

  five_level_data$economic_access_agriculture2market <- ordered(five_level_data$access_goods2market,
                                                                levels = c("No relationship", "Indirect relationship", "Direct relationship", "Strong relationship"))
  
  five_level_data$economic_access_labor2service_sector <- ordered(five_level_data$economic_access_labor2service_sector,
                                                                  levels = c("No relationship", "Indirect relationship", "Direct relationship", "Strong relationship"))

  five_level_data$economic_access_natural_resource_extraction <- ordered(five_level_data$economic_access_natural_resource_extraction,
                                                                         levels = c("No relationship", "Indirect relationship", "Direct relationship", "Strong relationship"))

  five_level_data$health_access_maternal_prenatal <- ordered(five_level_data$health_access_maternal_prenatal,
                                                             levels = c(" 1", " 2", " 3", " 4", " 5"))
  
  five_level_data$health_access_general_preventative <- ordered(five_level_data$health_access_general_preventative,
                                                                levels = c(" 1", " 2", " 3", " 4", " 5"))
  
  five_level_data$health_access_pediatric <- ordered(five_level_data$health_access_pediatric,
                                                     levels = c(" 1", " 2", " 3", " 4", " 5"))
  
  five_level_data$health_access_disease_chronic_condition <- ordered(five_level_data$health_access_disease_chronic_condition,
                                                                     levels = c(" 1", " 2", " 3", " 4", " 5"))
  
  five_level_data$health_access_emergency_trauma <- ordered(five_level_data$health_access_emergency_trauma,
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





# SAVE DATA FOR ANALYSIS --------------------------------------------------

  ## Use RData format to preserve ordered variables (CSV does not retain that information)
  save(data, file = '/Users/User/Dropbox/transport and conflict/Stones survey paper/Data/analysis.RData', ascii = TRUE)
