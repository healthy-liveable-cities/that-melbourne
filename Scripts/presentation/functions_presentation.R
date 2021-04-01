library(ggplot2)
library(dplyr) # for manipulating data
library(tidyr) # for pivoting data
library(scales) # for reordering factor for graphs
library(zoo) # for calculating rolling mean


############################## Graphs function (to do age and sex graphs) ################################################
diseaseLevels <- c("brsc","carc","dmt2","ishd","strk","tbalc","utrc")
diseaseLabels <- c("Breast cancer","Colon cancer","Diabetes type 2",
                   "Ischemic heart\ndisease","Stroke","Lung cancer",
                   "Uterine cancer")
auo_theme <- theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.key = element_blank(),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0, hjust=0),
        strip.background = element_rect(fill="white",size=0),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.8, "lines")) # use this to change the spacing between the different disease plots

########## Change in mode of transport
GraphsMode <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  dataFiltered <- output_transport_modes %>% 
    dplyr::filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(scenario=factor(scenario, levels=c("bl","sc"), labels=c("Baseline", "Scenario"))) %>%
    mutate(mode=factor(mode,
                       levels=c("walking","bicycle","public.transport","car","other"),
                       labels=c("Walking","Cycling","Public transport","Driving","Other")))
  
  ggplot(dataFiltered, aes(x=prop, y=mode, fill=scenario)) +
    geom_bar(stat="identity", position="dodge") + 
    # AUO teal and pink
    scale_fill_manual(values=c("#24C9AC","#EC4497")) +
    labs(x="Proportion of all trips") +
    geom_text(aes(label=paste0(round(prop*100,1),"%")),
              position=position_dodge(width=0.9),
              hjust=-0.05, vjust=0.5, size=5) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 5L),limits=c(0,.8)) +
    theme_classic() +
    theme(plot.title = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          axis.title.y=element_blank(),
          # axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
          legend.position = c(.99, .99),
          legend.justification = c(1,1),
          legend.title = element_blank(),
          legend.text = element_text(size=16),
          legend.key = element_blank())
}

GetMinutesText <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  dataFiltered1 <- PAall %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) 
  
  dataFiltered2 <- PAallGuide %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) 
  
  text <- cat("The shift from car travel for", dataFiltered1$scen, " ", "for", dataFiltered1$sex,
              " ", "adults aged", dataFiltered1$age, " ", "for the typical resident of Melbourne", 
              "increases walking from", dataFiltered1$walk_base, "to", dataFiltered1$walk_scen,
              "and cycling", dataFiltered1$cycle_base, "to", dataFiltered1$cycle_scen, "and increases those achieving 
  physical activity guidelines from", dataFiltered2$meets_base*100,"%", "to", dataFiltered2$meets_scen*100,"%")
  
  text
  
}

# diseasesExample$measure cases prevented
diseasesTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  dataFiltered <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(measure=case_when(measure=='inc_num' ~ 'Incidence',
                             measure=='mx_num'  ~ 'Deaths')) %>%
    filter(measure%in% c("Incidence", "Deaths")) %>%
    mutate(across(mean:percentile975, round, digits=1)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(population,measure,disease,mean,median,percentile025,percentile975)
}

HALYsTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  dataFiltered <- output_life_years_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(across(mean:percentile975, round, digits=1)) %>%
    dplyr::select(population,measure,mean,median,percentile025,percentile975)
}

diseasesChangeIncidenceTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc_percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975)
}

diseasesChangeIncidence <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc_percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(disease,median,percentile025,percentile975)
  
  ggplot(tmpPlot, aes(x=median, y=disease)) +
    geom_bar(stat="identity", color=NA, fill="#EC4497", # AUO pink
             position=position_dodge()) +
    labs(x="Percentage change diseases") +
    geom_errorbar(aes(xmin=percentile025, xmax=percentile975), width=.2) +
    scale_x_continuous(labels = scales::percent) +
    auo_theme +
    theme(axis.title.y=element_blank())
}

diseasesChangeDeaths <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="mx_percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(disease,median,percentile025,percentile975)
  
  ggplot(tmpPlot, aes(x=median, y=disease)) +
    geom_bar(stat="identity", color=NA, fill="#EC4497", # AUO pink
             position=position_dodge()) +
    labs(x="Percentage change deaths") +
    geom_errorbar(aes(xmin=percentile025, xmax=percentile975), width=.2) +
    scale_x_continuous(labels = scales::percent) +
    auo_theme +
    theme(axis.title.y=element_blank())
}

incidenceDiseasesGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc.num" & scenario== "diff") %>%
    dplyr::select(year,disease,median,percentile025,percentile975) %>%
    arrange(disease,year) %>%
    group_by(disease) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    ungroup() %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels))
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    facet_grid(rows=vars(disease), scales = "free_y") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Incidence") +
    auo_theme
}

mortalityDiseasesGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="mx.num" & scenario== "diff") %>%
    dplyr::select(year,disease,median,percentile025,percentile975) %>%
    arrange(disease,year) %>%
    group_by(disease) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    ungroup() %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels))
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    facet_grid(rows=vars(disease), scales = "free_y") +
    # facet_wrap(facets=vars(disease),ncol=1,scales="free_y") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Mortality") +
    auo_theme
}

halyGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="Lwx" & scenario== "diff") %>%
    dplyr::select(year,median,percentile025,percentile975) %>%
    arrange(year) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    # year 84 (the last year, has a weird uptick, removing for now)
    filter(year<=83)
  
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Health-adjusted life years") +
    auo_theme
}

lyGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="Lx" & scenario== "diff") %>%
    dplyr::select(year,median,percentile025,percentile975) %>%
    arrange(year) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    # year 84 (the last year, has a weird uptick, removing for now)
    filter(year<=83)
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Health-adjusted life years") +
    auo_theme
}