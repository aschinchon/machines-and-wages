# Load in libraries
library(dplyr)
library(tabulizer)
library(rlist)
library(readxl)
library(highcharter)

#############################################################################################
# Download and parse data about probability of computerisation
#############################################################################################

# Download pdf file
urlfile="https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf"
file="The_Future_of_Employment.pdf"
if (!file.exists(file)) download.file(urlfile, destfile = file, mode = 'wb')

# Extract tables using tabulizer (it takes some time)
out <- extract_tables(file, encoding="UTF-8")

# We are not interested in first two tables
list.remove(out, c(1:2)) -> table

# Parse table
prob_comput_df=data.frame()

for (i in 1:length(table))
{
  # We keep just SOC Code, rank and probability of computerisation
  # We also remove forst to lines of each element of table since they are non interesting
  table[[i]][-c(1,2),c(1,2,4)] %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    rbind(prob_comput_df) -> prob_comput_df
}

colnames(prob_comput_df) = c("rank", "probability", "soc")

# A bit of data cleaning
prob_comput_df %>% 
  mutate(rank=gsub("\\.","", rank) %>% as.numeric()) %>% 
  na.omit() -> prob_comput_df

file.remove(file)

#############################################################################################
# Download job satistics
#############################################################################################
# Download xlsx file
urlfile <- "https://www.bls.gov/emp/ind-occ-matrix/occupation.xlsx"
file <- "occupation.xlsx"
if (!file.exists(file)) download.file(urlfile, destfile = file, mode = 'wb')
job_stats_df <- read_excel(file, 
                       sheet="Table 1.7", 
                       skip=3,
                       col_names = c("job_title",
                                     "soc",
                                     "occupation_type",
                                     "employment_2016",
                                     "employment_2026",
                                     "employment_change_2016_26_nu",
                                     "employment_change_2016_26_pe",
                                     "self_employed_2016_pe",
                                     "occupational_openings_2016_26_av",
                                     "median_annual_wage_2017",
                                     "typical_education_entry",
                                     "work_experience_related_occ",	
                                     "typical_training_needed"))

file.remove(file)

#############################################################################################
# Join data frames and do the plot
#############################################################################################
results = prob_comput_df %>% 
  inner_join(job_stats_df, by = "soc") %>% 
  select(job_title, 
         probability, 
         employment_2016, 
         median_annual_wage_2017, 
         typical_education_entry) %>% 
  mutate(probability=as.numeric(probability),
         median_annual_wage_2017=as.numeric(median_annual_wage_2017),
         typical_education_entry=iconv(typical_education_entry, "latin1", "ASCII")) %>% 
  na.omit()


hchart(results, 
       "scatter", 
       hcaes(x = probability*100, 
             y = median_annual_wage_2017, 
             group=typical_education_entry, 
             size=employment_2016)) %>% 
  hc_title(text = "How Much Money Should Machines Earn?") %>%
  hc_subtitle(text = "Probability of Computerisation and Wages by Job") %>% 
  hc_credits(enabled = TRUE, text = "Source: Oxford Martin School and US Department of Labor") %>% 
  hc_xAxis(title = list(text = "Probability of Computerisation"), labels = list(format = "{value}%")) %>% 
  hc_yAxis(title = list(text = "Median Annual Wage 2017"), labels = list(format = "{value}$")) %>% 
  hc_plotOptions(bubble = list(minSize = 3, maxSize = 35)) %>% 
  hc_tooltip(formatter = JS("function(){
                            return ('<b>'+ this.point.job_title + '</b><br>'+
                            'Probability of computerisation: '+ Highcharts.numberFormat(this.x, 0)+'%' + 
                            '<br>Median annual wage 2017 ($): '+ Highcharts.numberFormat(this.y, 0) + 
                            '<br>Employment 2016 (000s): '+ Highcharts.numberFormat(this.point.size, 0) )}")) %>% 
  hc_chart(zoomType = "xy") %>%
  hc_exporting(enabled = TRUE)
