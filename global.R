#--------------------------------------------------
# TABLE OF CONTENTS
#--------------------------------------------------
# Section 1: Load Required Packages
#
# Section 2: Evelyn's Data Cleaning
#   1. Total Migration dataframe
#   2. Displacement dataframe
#   3. Climate Change dataframe
#   4. Political Climate dataframe
#   5. Economic Development dataframe
#
# Section 2: Fatema Creating Spatial Files
#   1. Destinations
#   2. Origins
#   3. IDPs
#   4. Yemen destinations
#   5. Syria destinations
#
# Section 3: Read in Data, Define Variables
#   1. Evelyn's Plots
#   2. Fatema Plots and Maps
#   3. Santoshi Plots
#   4. Kalyani Plots
#
#
#---------------------------------------------------




#=========================================================================================================
# Section 1: Load Required Packages 
#=========================================================================================================
library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(rgdal)
library(visNetwork)
library(leaflet)
library(dplyr)
library(rworldmap)

#=========================================================================================================
# Section 2: Evelyn's Data Cleaning
#=========================================================================================================

# ========== 1. Total Migration dataframe ===========
  df <- read_csv("migration.variables2.csv")
  
  #Rename some variables
  names(df) <- c("row_id",	"year",	"origin_country_cd",	"origin_country",	
                 "dest_country_cd",	"dest_country",	"distance",	"origin_region",	
                 "dest_region",	"num_refugees",	"asylum_change",	"idps",	"idps_change",
                 "pop",	"q1_temp",	"q2_temp",	"q3_temp",	"q4_temp",	"max_rain",	
                 "avg_rain",	"deaths_war",	"deaths_civ",	"deaths_tot",	"food_yield",
                 "gdp",	"cpi",	"autocracy",	"durable",	"percapinc")
  
  #Filter out missing values for origin_country
  df <- df %>%
    filter(!is.na(origin_country))

# ========== 2. Displacement dataframe ===========

  #Select only displacement variables, arranged in desired order
  df_displacement <- df %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:pop) %>%
    group_by(year, origin_country_cd, origin_country, origin_region, pop) %>%
    summarise(num_refugees = sum(num_refugees), asylum_change = sum(asylum_change),
              idps = sum(idps), idps_change = sum(idps_change)) %>%
    mutate(perc_refugees = (num_refugees/pop)*100, perc_idps = (idps/pop)*100) %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:perc_idps, pop) %>%
    arrange(desc(year), origin_country)
  
  #Write to csv file
  write_csv(df_displacement, "migration_displacement.csv")

# ========== 3. Climate Change dataframe ==========
  
  #Select only identifying variables and climate change variables, arranged in desired order
  df_climate <- df %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:avg_rain) %>%
    group_by(year, origin_country_cd, origin_country, origin_region, pop, 
             q1_temp, q2_temp, q3_temp, q4_temp, max_rain, avg_rain) %>%
    summarise(num_refugees = sum(num_refugees), asylum_change = sum(asylum_change),
              idps = sum(idps), idps_change = sum(idps_change)) %>%
    mutate(perc_refugees = (num_refugees/pop)*100, perc_idps = (idps/pop)*100) %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:perc_idps, pop:avg_rain) %>%
    arrange(desc(year), origin_country)
  
  #Write to csv file
  write_csv(df_climate, "migration_climate.csv")

# ========== 4. Political Climate dataframe ==========
  
  #Select only identifying variables and political variables, arranged in desired order
  df_political <- df %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:pop, 
                  deaths_war:deaths_tot, autocracy, durable) %>%
    group_by(year, origin_country_cd, origin_country, origin_region, pop, 
             deaths_war, deaths_civ, deaths_tot, autocracy, durable)  %>%
    summarise(num_refugees = sum(num_refugees), asylum_change = sum(asylum_change),
              idps = sum(idps), idps_change = sum(idps_change)) %>%
    mutate(perc_refugees = (num_refugees/pop)*100, perc_idps = (idps/pop)*100) %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:perc_idps, pop:durable) %>%
    arrange(desc(year), origin_country)
  
  #Write to csv file
  write_csv(df_political, "migration_political.csv")

# ========== 5. Economic Development dataframe ==========
  #Select only identifying variables and economic variables, arranged in desired order
  df_economic <- df %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:pop, 
                  food_yield, gdp, cpi, percapinc) %>%
    group_by(year, origin_country_cd, origin_country, origin_region, pop, 
             food_yield, gdp, cpi, percapinc)  %>%
    summarise(num_refugees = sum(num_refugees), asylum_change = sum(asylum_change),
              idps = sum(idps), idps_change = sum(idps_change)) %>%
    mutate(perc_refugees = (num_refugees/pop)*100, perc_idps = (idps/pop)*100) %>%
    dplyr::select(year:origin_country, origin_region, num_refugees:perc_idps, pop:percapinc) %>%
    arrange(desc(year), origin_country)
  
  df_economic <- df_economic %>%
    dplyr::select(year, origin_country, origin_region, num_refugees, idps, 
                  perc_refugees, perc_idps, pop, gdp) %>%
    mutate(`GDP per Capita (Thousands)` = gdp/(1000*pop)) %>%
    mutate(Region = case_when(
      origin_region %in% grep('Africa', origin_region, value=TRUE)     ~ "Africa",
      origin_region %in% grep('Asia', origin_region, value=TRUE)       ~ "Asia",
      origin_region %in% grep('Europe', origin_region, value=TRUE)     ~ "Europe",
      origin_region %in% c("Central America", "Northern America")      ~ "North America",
      origin_region %in% c("Micronesia", "Polynesia", "Melanesia")     ~ "Oceania",
      TRUE                                                             ~ origin_region
    ))
  
  #Write to csv file
  write_csv(df_economic, "migration_economic.csv")
  
#=========================================================================================================
# Section 3: Fatema's Spatial Files
#=========================================================================================================
  # ========== 1. Destinations: Creating spatial files ==========
  ref<-read.csv("ref.csv")
  ref.dest<- ref%>%group_by("ISO3"=iso3_destination)%>%summarise("Total"=sum(refugee_asylum_change))
  ref.dest$proportion<- (ref.dest$Total/sum(ref.dest$Total))*100
  ref.dest <- joinCountryData2Map(ref.dest, joinCode = "ISO3", nameJoinColumn = "ISO3")
  
  # ========== 2. Origins: Creating spatial files ==========
  ref.orig<- ref%>%group_by("ISO3"=iso3_origin)%>%summarise("Total"=sum(refugee_asylum_change))
  ref.orig$proportion<- (ref.orig$Total/sum(ref.orig$Total))*100
  ref.orig <- joinCountryData2Map(ref.orig, joinCode = "ISO3", nameJoinColumn = "ISO3")
  
  # ========== 3. IDPs: Creating spatial files ==========
  idp.only<-read.csv("idp.only.csv")
  idps<- idp.only%>%group_by("ISO3"=iso3)%>%summarise("Total"=sum(idps_change))
  idps$proportion<- (idps$Total/sum(idps$Total))*100
  idps <- joinCountryData2Map(idps, joinCode = "ISO3", nameJoinColumn = "ISO3")
  
  # ========== 4. Syria destinations: Creating spatial files ==========
  syr.ref<-ref[ref$iso3_origin=="SYR",]
  syr.ref<-syr.ref%>%group_by("ISO3"=iso3_destination)%>%summarise("Total"=sum(refugee_asylum_change))%>%filter(Total>0)
  syr.ref$proportion <- (syr.ref$Total/sum(syr.ref$Total))*100
  syr.ref <- joinCountryData2Map(syr.ref, joinCode = "ISO3", nameJoinColumn = "ISO3")

  # ========== 5. Yemen destinations: Creating spatial files ==========
  
  yem.ref<-ref[ref$iso3_origin=="SYR",]
  yem.ref<-yem.ref%>%group_by("ISO3"=iso3_destination)%>%summarise("Total"=sum(refugee_asylum_change))%>%filter(Total>0)
  yem.ref$proportion <- (yem.ref$Total/sum(yem.ref$Total))*100
  yem.ref <- joinCountryData2Map(yem.ref, joinCode = "ISO3", nameJoinColumn = "ISO3")
    
#=========================================================================================================
# Section 4: Read in Data, Define Variables
#=========================================================================================================

# ========== 1. Evelyn's Plots ==========
  #Overview Tab
  df_displacement <- read_csv("migration_displacement.csv")
  
  df_tot_displacement <- df_displacement %>%
    group_by(year) %>%
    summarize(`Refugees (Millions)` = sum(num_refugees, na.rm= TRUE)/1000000, 
              `IDPS (Millions)` = sum(idps, na.rm=TRUE)/1000000) 
  
  #Explore On Your Own Tab
  df_economic <- read_csv("migration_economic.csv") %>%
    group_by(year, origin_country, perc_refugees, perc_idps)
  
  countries_vec <- unique(df_displacement$origin_country)
  regions_vec <- unique(df_economic$Region)

# ========== 2. Fatema's Plots ==========
  #import data
  disp.new <- read.csv("dispnew.csv")
  s.mapped <- readRDS("./s.mapped.rds")
  y.mapped <- readRDS("./y.mapped.rds")
  ref.dest <- readRDS("./ref.dest.rds")
  ref.orig <- readRDS("./ref.orig.rds")
  idps<- readRDS("./idps.rds")
  overview<- read.csv("overview.csv")
  
  #prepare variables for plotting
  overview<-overview%>%filter(Year>1959)
  overview$Total<-ceiling(overview$Total/1000000)
  disp.new$Value<-ceiling(disp.new$Value/1000000)
  s.mapped$GDPpc<-s.mapped$GDP_MD_EST*1000000/s.mapped$POP_EST
  y.mapped$GDPpc<-y.mapped$GDP_MD_EST*1000000/y.mapped$POP_EST  

# ========== 3. Santoshi's Plots ==========    
  
  ## world country info 
  world = readOGR(dsn = "country_centroids_az8", layer = "country_centroids_az8")
  #world = readOGR("country_centroids_az8/country_centroids_az8.shp")
  
  world@data = world@data %>% 
    filter(pop_est > 0) ## remove negative pop est
  ## refugee flow data
  data = read_csv("migration.variables.csv")
  
  ## join country data and refugee flow data
  join = left_join(data, world@data, by = c("iso3_origin" = "iso_a3"))
  join = join %>% ## first join origin 
    select(year, iso3_origin, origin.country,
           iso3_destination, destination.country, 
           refugees, Longitude, Latitude, pop_est,
           continent) %>% 
    rename(origin_lat = Latitude, 
           origin_long = Longitude, 
           origin_pop = pop_est, 
           orig_cont = continent)
  ## then join destination-related data
  join = left_join(join, world@data[,c("iso_a3", "Longitude", 
                                       "Latitude", "pop_est", "continent")], 
                   by = c("iso3_destination" = "iso_a3")) %>% 
    rename(dest_lat = Latitude, 
           dest_long = Longitude, 
           dest_pop = pop_est, 
           dest_cont = continent)
  ## add decade variable
  join = join %>% 
    mutate(decade = case_when(
      year >= 1960 & year < 1970 ~ "1960s",
      year >= 1970 & year < 1980 ~ "1970s",
      year >= 1980 & year < 1990 ~ "1980s",
      year >= 1990 & year < 2000 ~ "1990s",
      year >= 2000 & year < 2010 ~ "2000s",
      year >= 2010 & year < 2020 ~ "2010s"
    )) 
  
  ## do second network for top refugee countries of each - use avg refugees over the time period
  
  ## function to visualise networks
  visualise = function(n, decade) {
    decade = enquo(decade)
    ## top refugees per capita for specified decade
    filtered = join %>% 
      filter(decade == !!decade) %>%
      group_by(iso3_origin) %>% 
      summarise(rpc = sum(refugees, na.rm = TRUE)/origin_pop[1]) %>% 
      top_n(n, rpc)
    ## top destinations by these countries
    top1 = join %>% 
      filter(decade == !!decade) %>% 
      filter(iso3_origin %in% filtered$iso3_origin) %>% 
      group_by(iso3_origin, iso3_destination) %>% 
      summarise(refugees = sum(refugees)) %>% 
      ungroup() %>% 
      group_by(iso3_origin) %>% 
      top_n(5, refugees)
    ## filtering only countries with highest refugee per capita and their top recipients
    join_top = top1 %>% 
      select(-refugees) %>% 
      left_join(join, by = c("iso3_origin", "iso3_destination")) %>% 
      filter(decade == !!decade)
    ## countries in refugee network
    countries = c(join_top$iso3_origin, join_top$iso3_destination) %>% 
      unique()
    ## calculate total refugees per country
    rpc1 = join %>% 
      filter(decade == !!decade) %>% 
      group_by(iso3_origin) %>% 
      summarise(refugees = sum(refugees)) 
    rpc2 = join %>% 
      filter(decade == !!decade) %>%
      group_by(iso3_destination) %>% 
      summarise(refugees = sum(refugees))  
    rpc = full_join(rpc1, rpc2, by = c("iso3_origin" = "iso3_destination")) %>% 
      rename(country = iso3_origin) %>% 
      replace_na(list(refugees.x = 0, refugees.y = 0)) %>% 
      mutate(net_ref = refugees.x - refugees.y) %>% ## net refugees as some countries are both origin and dest
      filter(country %in% countries) 
    ## nodes
    nodes = world@data %>% 
      filter(iso_a3 %in% countries) %>% 
      left_join(rpc, by = c("iso_a3" = "country")) %>% 
      select("iso_a3", "name", "pop_est", "refugees.x", "refugees.y", "net_ref", "continent") %>% 
      mutate(
        label = name, 
        value = abs(net_ref/pop_est),
        title = ifelse(net_ref/pop_est < 0, paste0(round(refugees.y/pop_est*1000), 
                                                   " refugees per 1000"), 
                       paste0(round(refugees.x/pop_est*1000), " refugees per 1000")),
        shape = ifelse(net_ref/pop_est < 0, "triangle", "square"),
        group = continent, 
        font = "14px arial black") %>% 
      rowid_to_column("id")
    ## edges: from, to, width
    edges = join_top %>% 
      group_by(iso3_origin, iso3_destination) %>% 
      summarise(flow = sum(refugees)) %>% 
      left_join(nodes, by = c("iso3_origin" = "iso_a3")) %>% 
      rename(origin_id = id) %>% 
      left_join(nodes, by = c("iso3_destination" = "iso_a3")) %>%
      rename(dest_id = id) %>% 
      transmute(from = origin_id, to = dest_id, arrows = "to")
    ## legend
    lnodes = data.frame(label = c(paste("Refugee", "\n", "Recipient"), paste("Refugee","\n", "Producer")),
                        shape = c( "triangle", "square"), color.border = c("grey", "grey"), color.background=c("white","white"),
                        title = "Shape", id = 1:2)
    ## network graph
    visNetwork(nodes, edges) %>% 
      visGroups(groupname = "Africa", color = "lightgreen") %>% 
      visGroups(groupname = "Asia", color = "khaki") %>% 
      visGroups(groupname = "Europe", "dodgerblue3") %>% 
      visGroups(groupname = "North America", color = "lightpink") %>% 
      visGroups(groupname = "Oceania", color = "thistle") %>% 
      visLegend(addNodes = list()) %>% 
      visLayout(randomSeed = 124) %>% 
      visLegend(addNodes = lnodes, width = 0.15) %>% 
      visInteraction()
  }
  
  # ========== 3. Kalyani's Plots ==========  

  mig = read.csv("./migration.variables2.csv")
  variable = 'pop'
  mig1 = subset(mig, year == 2017)
  head(mig1$refugees)
  mig_ref = aggregate(x=mig1$refugees, by=list(origin=mig1$origin.country), FUN=sum)
  mig_ref
  mig_ref = as.data.frame(mig_ref)
  names(mig_ref) = c('country', 'refugees')
  head(mig_ref)
  mig2 = aggregate(x=mig1[variable], by=list(origin=mig1$origin.country), FUN=mean)
  head(mig2)
  mig2 = as.data.frame(mig2)
  mig_ref$var = mig2$x
  head(mig_ref)