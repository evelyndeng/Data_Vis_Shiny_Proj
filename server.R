
server <- function(input, output) {

# ================================================== 1. Overview Tab ====================================================

  #Overview Tab: text 1
  output$text_overview1 <- renderText({
    paste("<h2>Overview<h2>",
          "<h5>Forced displacement has transformed significantly over time, whether in terms of the total number of displaced, the type of displacment, or the destinations of people on the move.<h5>", 
          "<br>",
          "The year 2017 marked the 6th consecutive year when the number of forcibly displaced people in the world hit a post WWII record. The figure increased by 3 million in 2017 to reach 68 million displaced people.",
          "<br>", "<br>",
          "Causes differ and include persecution, conflict, human rights violations, generalised violence, poverty, and climate change.",
          "<br>", "<br>",
          "The chart below highlights the severity of the issue, particularly over the last few years.",
          "<br>", "<br>",
          "This project highlights the phases of migration and destinations of refugees over the years. It further offers a platform for some exploration by the user to look for patterns in migration accross countries and considering several variables.",
          "<br>", " ")
  })
  
  #Overview Tab: toal global refugee displacement plot
  output$tot_displaced <- renderPlotly({ 
    ggplotly(ggplot(overview, aes(x=Year, y=Total, color=Displacement)) +
               scale_color_manual(name="Type of Displacement", values=c("red", "blue", "green", "purple")) +
               geom_line() +
               ylab("Displaced Persons (millions)") +
               theme_hc() + 
               ggtitle("Displaced Persons (1960-2016)"))
  })
  
  output$text_overview2 <- renderText({
    paste("<br>","<br>","<br>")
  })
    
  
# =========================================== 2. Mapping Displacement Tab ================================================

# =========================================== 2.1 Top Destinations ================================================
  
    output$text_mapping_displacement1<-renderText({
    paste("<h2>Which countries have hosted the largest proportions of the world's refugees and asylum seekers?</h2>",
          "<h3>Aggregated starting from 1960<h3>", 
          "<br>"," ")
  })
  output$map_mapping_displacement1<- renderLeaflet({
    d.bins<-c(0,2,4,6,8,10,12)
    d.pal<-colorBin("Oranges", domain=ref.dest$proportion, bins=d.bins, na.color="lightgrey")
    d.content <- paste("Country:",ref.dest$ADMIN,"<br/>",
                       "Total Refugees:",ref.dest$Total,"<br/>",
                       "Proportion of World:", paste0(ceiling(ref.dest$proportion),"%"), "<br/>")
    dest.map<-leaflet(ref.dest) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(
        fillColor=~d.pal(proportion),
        weight=0.5,
        color="grey",
        fillOpacity=0.8, 
        popup=d.content) %>%
      setView(0,20,zoom=1.5) %>%
      addLegend("topright", pal=d.pal, values=~proportion, title="Proportion of all refugees (%)", labFormat=labelFormat(prefix="%"), opacity=1)
    print(dest.map)
  })
  output$text_mapping_displacement2<- renderText({
    paste("*Note that this map is biased to more recent waves of migration, since it subtracts refugees that have returned. The dataset is also better at capturing refugees in developed countries since there are more rigid systems of accounting there.",
          "<br>","<br>","<br>")
  })
  
  # =========================================== 2.2 Main Origins ================================================
  
  output$text_mapping_displacement3<-renderText({
    paste("<h2>Which countries have produced the largest proportions of the world's refugees and asylum seekers?</h2>",
          "<h3>Aggregated starting from 1960<h3>", 
          "<br>"," ")
  })
  output$map_mapping_displacement2<- renderLeaflet({
    o.bins<-c(0,3,6,9,12,15,18)
    o.pal<-colorBin("Oranges", domain=ref.orig$proportion, bins=o.bins, na.color="lightgrey")
    o.content <- paste("Country:",ref.orig$ADMIN,"<br/>",
                       "Total Refugees:",ref.orig$Total,"<br/>",
                       "Proportion of World:", paste0(ceiling(ref.orig$proportion),"%"), "<br/>")
    orig.map<-leaflet(ref.orig) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(
        fillColor=~o.pal(proportion),
        weight=0.5,
        color="grey",
        fillOpacity=0.8, 
        popup=o.content) %>%
      setView(0,20,zoom=1.5) %>%
      addLegend("topright", pal=o.pal, values=~proportion, title="Proportion of all refugees (%)", labFormat=labelFormat(prefix="%"), opacity=1)
    print(orig.map)
  })
  output$text_mapping_displacement4<-renderText({
    paste("*Note that this map is biased to more recent waves of migration, since it subtracts refugees that have returned. The dataset is also better at capturing refugees in developed countries since there are more rigid systems of accounting there.",
          "<br>","<br>","<br>")
  })

# =========================================== 2.3 Internally Displaced People ================================================
  
  output$text_mapping_displacement5<-renderText({
    paste("<h2>Which countries have had the largest proportions of worldwide internal displacement?</h2>",
          "<h3>Aggregated starting from 1960<h3>", 
          "<br>"," ")
  })
  output$map_mapping_displacement3<- renderLeaflet({
    i.bins<-c(0,3,6,9,12,15,18,21)
    i.pal<-colorBin("Oranges", domain=idps$proportion, bins=i.bins, na.color="lightgrey")
    i.content <- paste("Country:",idps$ADMIN,"<br/>",
                       "Total Refugees:",idps$Total,"<br/>",
                       "Proportion of World:", paste0(ceiling(idps$proportion),"%"), "<br/>")
    idp.map<-leaflet(idps) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(
        fillColor=~i.pal(proportion),
        weight=0.5,
        color="grey",
        fillOpacity=0.8, 
        popup=i.content) %>%
      setView(0,20,zoom=1.5) %>%
      addLegend("topright", pal=i.pal, values=~proportion, title="Proportion of IDPs worldwide (%)", labFormat=labelFormat(prefix="%"), opacity=1)
    print(idp.map)
  })
  output$text_mapping_displacement6<-renderText({
    paste("*Note that this map is biased to more recent internal displacement, since it subtracts people that have changed their status. The dataset is also better at capturing refugees in countries affected by conflict since IDP statistics tend to primarily be gathered in conflict-affected countries.",
          "<br>","<br>","<br>")
  })
  # ================================== 3.0 Phases of Refugee Migration Tab: Introduction ======================================
  
  #Phases Tab: intro text 1
  output$text_phases_intro1 <- renderText({
    paste("<h2>Understanding The Patterns of Refugee Migration since 1960</h2>", 
          "In a series of network graphs, we visualise the countries which produced the highest numbers of refugees per capita in each decade since 1960, and the top destination countries for those refugees.",
          "<br>", "<br>",
          "How can we characterise the different waves of refugee migration since 1960? What were the geographical patterns in migration?",
          "<h3>Learn about those waves under this section's tabs!<h3>",
          "<br>", " ") 
  })
  
  # ================================== 3.1 Phases of Refugee Migration Tab: First Phase ======================================
  
  #Phases Tab: first phase text 1
  output$text_phases_first1 <- renderText({
    paste("<h2>First Phase: African Decolonization</h2>",
          "<h3>The 1960s</h3>",
          "The five countries with the highest rates of refugees in the 1960s were: Guinea Bissau, Angola, Rwanda, Sudan and Mozambique.",
          "<br>", "<br>",
          "The shape of the nodes represents whether a country was a net recipient of refugees in the given decade (triangle), or a net producer of refugees (square). The size of the nodes is a function of the number of refugees exiting from the country if it was was a net producer of refugees, or coming into the country if it was a net recipient.",
          "<br>", "<br>",
          "From the interactive graph below, we can see that refugee migration in the aftermath of African decolonization was mostly intracontinental. Ethiopia stands out as a country with a high number of refugees produced in the 1960s but which also hosted refugees from other African states. The Democratic Republic of Congo received refugees from three of the top five refugee-producing countries.",
          "<br>","<br>",
          "Feel free to ZOOM IN!") 
  })
  
  #Phases Tab: first phase plot 1
  output$plot_phases_first1 <- renderVisNetwork({
    visualise(5, decade = "1960s")
  })
  
  #Phases Tab: first phase text 2
  output$text_phases_first2 <- renderText({
    paste("<h3>The 1970s</h3>",
          "The five countries with the highest rates of refugees in the 1970s were: Equatorial Guinea, Angola, Guinea Bissau, Rwanda and Gambia.",
          "<br>", "<br>",
          "The shape of the nodes represents whether a country was a net recipient of refugees in the given decade (triangle), or a net producer of refugees (square). The size of the nodes is a function of the number of refugees exiting from the country if it was was a net producer of refugees, or coming into the country if it was a net recipient.",
          "<br>", "<br>",
          "Again, migration was intracontinental - refugees moved to neighbouring countries. Burundi was a net recipient of refugees in the decade but it also produced the highest rate of refugees in the same decade.",
          "<br>", " ") 
  })
  
  #Phases Tab: first phase plot 2
  output$plot_phases_first2 <- renderVisNetwork({
    visualise(5, decade = "1970s")
  })
  output$blank_phase1 <- renderText({
    paste("<br>","<br>","<br>")
  })
  
  # ================================== 3.2 Phases of Refugee Migration Tab: Second Phase ======================================
  
  #Phases Tab: second phase text 1
  output$text_phases_second1 <- renderText({
    paste("<h2>Second Phase: Wars in Afghanistan and Central America</h2>",
          "<h3>The 1980s</h3>",
          "The five countries with the highest rates of refugees in the 1980s were: Afghanistan, Rwanda, Namibia, Angola and El Salvador.",
          "<br>", "<br>",
          "In addition to intracontinental refugee flows, there was intercontinental migration of refugees from Afghanistan and Angola to the US in the 1980s.",
          "<br>", " ") 
  })
  
  #Phases Tab: second phase plot 1
  output$plot_phases_second1 <- renderVisNetwork({
    visualise(5, decade = "1980s")
  })
  output$blank_phase2 <- renderText({
    paste("<br>","<br>","<br>")
  })
  
  # ================================== 3.3 Phases of Refugee Migration Tab: Third Phase ======================================
  
  #Phases Tab: third phase text 1
  output$text_phases_third1 <- renderText({
    paste("<h2>Third Phase: Europe Joins the Fray</h2>",
          "<h3>The 1990s</h3>",
          "<br>",
          "While Afghanistan continued to have one of the highest refugee outflows, the conflict following the breakup of Yugoslavia resulted in Bosnia and Herzegovina becoming the country with the highest rate of refugee outflow in the 1980s. Central American countries no longer had high rates of refugee outflows by the 90s.",
          "<br>", "<br>",
          "Intercontinental refugee movement was more prominent in this decade: Four of the top five destinations of Bhutanese refugees were in other continents. The US continued to be one of the top destinations for African, Bosnian and Afghan refugees.",
          "<br>", "<br>",
          "<br>", " ") 
  })
  
  #Phases Tab: third phase plot 1
  output$plot_phases_third1 <- renderVisNetwork({
    visualise(5, decade = "1990s")
  })
  output$blank_phase3 <- renderText({
    paste("<br>","<br>","<br>")
  })
  
  # ================================== 3.4 Phases of Refugee Migration Tab: Fourth Phase ======================================
  
  #Phases Tab: fourth phase text 1
  output$text_phases_fourth1 <- renderText({
    paste("<h2>Fourth Phase: Instability in the Middle East</h2>",
          "<h3>The 2000s</h3>",
          "High refugee outflows continue through the 2000s in Bhutan, Afghanistan, Liberia, and Bosnia and Herzegovina. European countries like the Netherlands and Germany step up as top recipient countries for these countries.",
          "<br>", "<br>",
          "Palestinean refugees from the escalating Palestinean conflict largely remain in the Middle East.",
          "<br>", " ") 
  })
  
  #Phases Tab: fourth phase plot 1
  output$plot_phases_fourth1 <- renderVisNetwork({
    visualise(5, decade = "2000s")
  })
  
  #Phases Tab: fourth phase text 2
  output$text_phases_fourth2 <- renderText({
    paste( 
      "<h3>The 2010s</h3>",
      "The five countries with the highest rates of refugees in from 2010 to 2017 were: Syria, Somalia, Afghanistan, the Central African Republic and South Sudan. African refugees remain primarily in Africa.",
      "<br>", "<br>",
      "The US no longer features as a top recipient country for the countries with the most severe refugee problems.",
      "<br>", " ")
  })
  
  #Phases Tab: fourth phase plot 2
  output$plot_phases_fourth2 <- renderVisNetwork({
    visualise(5, decade = "2010s")
  })
  
  output$blank_phase4 <- renderText({
    paste("<br>","<br>","<br>")
  })
  
# ================================================ 4. A Case Study Tab ===================================================
  
  output$text_case_study1 <- renderText({
    paste("<h2>When do displaced people leave their countries?</h2>",
          "<h3>Lets look at the cases of Yemen and Syria, which only started to experience displacement over the last decade.</h3>",
          "<br>",
          "But first, some context... Syria and Yemen both experienced large waves of protests in 2011, in what is often called the 'Arab Spring.' Both countries descended into war soon thereafter. While Syria's war started in 2012, Yemen's officially began in 2015.  ",
          "<br>", "<br>",
          "Despite those similarities, the two countries are also very different. Syria is wealthier than Yemen. It does not have a recent history of conflict, while different parts of Yemen have experienced up to 13 periods of substantial conflict since 1994 alone.",
          "<br>", "<br>",
          "From the interactive graph below, we can see that Yemenis did not start leaving Yemen until very recently and a relatively small scale, while in Syria people left very soon after the war. Most Yemenis have remained internally displaced, while Syrian's have an almost equal number of refugees and internally displaced people at this point.",
          "<br>","<br>", " ")
  })
  output$plot_case_study1<- renderPlotly({
    p<-print(ggplotly(ggplot(disp.new, aes(x=Year, y=Value, color=Country, linetype=Displaced)) +
                        geom_line() +
                        scale_color_manual(name="Country,", values=c("#F768A1","#78C679")) +
                        scale_linetype_manual(name="Type of displacement", values=c("solid", "dashed"))+
                        ylab("Displaced People (millions)") + 
                        theme_hc() + 
                        ggtitle("Forced Displacement in Syria and Yemen (2000-2017)")))
  })
  output$text_case_study2<-renderText({
    paste("<h2>Where do they go?</h2>",
          "<h3>Lets compare the destinations of all Yemeni and Syrian refugees since 2000. How are they different?</h3>",
          "<br>", 
          "Part of the explanation may relate to the income levels of neighboring countries that are accessible to them. Inspect GDP per capita in each country's top destinations.",
          "<br>","<br>", " ",
          "<h3>Yemenis<h3>")
  })
  output$map_case_study1<- renderLeaflet({
    y.bins<-c(0,5,10,15,20,25)
    y.pal<-colorBin("RdPu", domain=y.mapped$proportion, bins=y.bins, na.color="lightgrey")
    y.content <- paste("Destination:",y.mapped$ADMIN,"<br/>",
                       "Total Yemeni refugees:",y.mapped$total,"<br/>",
                       "Proportion:", paste0(ceiling(y.mapped$proportion),"%"), "<br/>",
                       "GDP per Capita", paste0(ceiling(s.mapped$GDPpc)), "<br/>")
    yem.dest<-leaflet(y.mapped) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(
        fillColor=~y.pal(proportion),
        weight=0.5,
        color="grey",
        fillOpacity=0.8, 
        popup=y.content) %>%
      setView(0,40,zoom=1.5) %>%
      addLegend("topright", pal=y.pal, values=~proportion, title="Proportion of Yemeni refugees (%)", labFormat=labelFormat(prefix="%"), opacity=1)
    print(yem.dest)
  })
  output$text_case_study3<- renderText({
    paste("<br>","<br>",
      "<h3>Syrians<h3>")
  })
  output$map_case_study2<- renderLeaflet({
    s.bins<-c(0,5,10,15,40,50)
    s.pal<-colorBin("YlGn", domain=s.mapped$proportion, bins=s.bins, na.color="lightgrey")
    s.content <- paste("Destination:",s.mapped$ADMIN,"<br/>",
                       "Total Syrian refugees:",s.mapped$total,"<br/>",
                       "Proportion:", paste0(ceiling(s.mapped$proportion),"%"), "<br/>",
                       "GDP per Capita", paste0(ceiling(s.mapped$GDPpc)), "<br/>")
    syr.dest<-leaflet(s.mapped) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(
        fillColor=~s.pal(proportion),
        weight=0.5,
        color="grey",
        fillOpacity=0.8, 
        popup=s.content) %>%
      setView(0,40,zoom=1.5) %>%
      addLegend("topright", pal=s.pal, values=~proportion, title="Proportion of Syrian refugees (%)", labFormat=labelFormat(prefix="%"), opacity=1)    
    print(syr.dest)
  })
  
  output$text_case_study4<- renderText({
    paste("<br>", "<br>")
  })

  
# =================================== 5.1 Explore: Displacement Over Time Sub-Tab ========================================
  
  #Displacement Over Time Tab: text 1
  output$text_disp1 <- renderText({
    paste("<h2>Displacement Over Time</h2>",
          "How can we explore the amount of displacement within a country? First, let's look at the percentage of the total population that is displaced.",  
          "<br>", "<br>",
          "Select at least one country on the left panel to begin your exploration!",
          "<br>", "<br>", " ") 
  })
  
  #Displacement Over Time Tab: percent displacement plot
  output$displacement_perc <- renderPlot({ 
    ggplot(filter(df_displacement, origin_country %in% input$countries),
           aes(x = year, y = perc_refugees, group = origin_country)) + 
      geom_line(aes(color = origin_country), size = 1.2, alpha = 0.3) +
      labs(title = "Percent of Total Population Displaced Over Time",
           x="", y="Percent Displaced", color = "Country") + 
      scale_x_continuous(limits= c(1960, 2018), breaks=seq(1960, 2010, 10)) +
      theme_hc()
  })
  
  #Displacement Over Time Tab: text 2
  output$text_disp2 <- renderUI({
    HTML(paste(" ", " ", 
               "Below are the same country data for the total number of people displaced. Note how comparisons between countries differ for total numbers versus percentage of population.", 
               "<br>","<br>",
               "For example, select Cambodia and Viet Nam as two comparison countries. What do you find?",
               " ", " ", sep = "<br/>"))
  })
  
  #Displacement Over Time Tab: total displacement plot
  output$displacement <- renderPlot({ 
    ggplot(filter(df_displacement, origin_country %in% input$countries), 
           aes(x = year, y = num_refugees, group = origin_country)) + 
      geom_line(aes(color = origin_country), size = 1.2, alpha = 0.3) +
      labs(title = "Number Displaced Over Time",
           x="", y="Number Displaced (000's)", color = "Country") +
      scale_x_continuous(limits= c(1960, 2018), breaks=seq(1960, 2010, 10)) +
      theme_hc()
  })
# =================================== 5.2 Explore: Economics and Displacement Sub-Tab ========================================
  
  #Economics and Displacement Tab: text 1
  
  output$text_econ1 <- renderText({
    paste("<h2>Economics and Displacement</h2>",
          "How do countries with different GDPs differ in levels of displacement? Select at least one region in the left panel to find out.",          
          "<br>", "<br>", " ") 
  })
  
  #Economics and Displacement Tab: Per Capita GDP plot
  output$gdp <- renderPlotly({ 
    ggplotly(ggplot(filter(df_economic, Region %in% input$regions, year %in% input$year), 
           aes(x = `GDP per Capita (Thousands)`, y = perc_refugees)) + 
      geom_point(aes(color = Region), size = 2, alpha = 0.3) +
      scale_y_continuous(limits=c(0, 14), breaks=seq(0, 12, 3)) +
      scale_x_continuous(limits=c(0, 110)) +
      labs(x="Per Capita GDP (Thousands)", y="Percent Displaced", title = "Per Capita GDP and Percent of Total Population Displaced") +
      theme_hc())
  })
  
# =================================== 6.3 Explore: Explore more variables ========================================
  
    
  output$scatter <- renderPlotly({
    variable1 = as.character(input$variable1)
    mig$variable = mig[,variable1]
    mig1 = subset(mig, year == input$year)
    mig_ref = aggregate(x=mig1$refugees, by=list(origin=mig1$origin.country), FUN=sum)
    mig_ref = as.data.frame(mig_ref)
    names(mig_ref) = c('country', 'refugees')
    mig2 = aggregate(x=mig1$variable, by=list(origin=mig1$origin.country), FUN=mean)
    mig2 = as.data.frame(mig2)
    mig_ref$var = mig2$x
    
    x <- list(
      title = "Variable")
    y <- list(
      title = "Refugees")
    
    
    plot_ly(type = 'scatter', data= mig_ref,x = ~var, y = ~refugees,text = mig_ref$country,
            hoverinfo = 'text',
            marker = list(color='green'),
            showlegend = F
    ) %>%
      layout(xaxis = x, yaxis = y)
  })
  
}

