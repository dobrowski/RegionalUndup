
# https://github.com/dobrowski/RegionalUndup.git 


# The number of districts with an 80% unduplicated student count in each county,
# The number of schools and charter schools with an 80% unduplicated student count in each county.
# The number of pupils that are unduplicated per county.

library(MCOE)
library(tidyverse)
library(googlesheets4)


con <- mcoe_sql_con()

counties <- c("Monterey", "San Benito", "Santa Cruz", "San Luis Obispo", "Santa Barbara", "Ventura")

sheet <- "https://docs.google.com/spreadsheets/d/1ygKrQPXkyQDSUcMfxvCwxW1PNsoCJzKuDABGhzlRPXQ/edit#gid=0"

upc <- tbl(con, "UPC") %>% 
    filter(county_name %in% counties,
           #        DistrictCode == "10272",
           academic_year == "2021-2022"
    ) %>%
    #     head(20) %>%
    collect()



upc.revise <- upc %>%
    mutate(perc.undup = 100*calpads_unduplicated_pupil_count_upc/total_enrollment) 

upc.district <- upc %>%
   # filter(charter_school_y_n == "N" | is.na( charter_school_y_n) ) %>% 
    group_by(academic_year, county_name ,district_name) %>%
    summarise(total = sum(total_enrollment),
              undup = sum(calpads_unduplicated_pupil_count_upc),
              perc.dist = 100*undup/ total)








upc.revise %>%
    filter(perc.undup >= 50) %>%
    sheet_write(ss = sheet,
            sheet = "Schools 50 percent Undup"
            )


upc.revise %>%
    filter(perc.undup >= 80) %>%
    sheet_write(ss = sheet,
                sheet = "Schools 80 percent Undup"
    )



upc %>%
    group_by(academic_year, county_name) %>%
    summarise(sum(calpads_unduplicated_pupil_count_upc))%>%
    sheet_write(ss = sheet,
                sheet = "Student Count by County"
    )



upc.revise %>%
    filter(perc.undup >= 50) %>%
    group_by(academic_year, county_name) %>%
    summarise(n()) %>%
    sheet_write(ss = sheet,
                sheet = "School Count by County 50 percent Undup"
    )


upc.revise %>%
    filter(perc.undup >= 80) %>%
    group_by(academic_year, county_name) %>%
    summarise(n()) %>%
    sheet_write(ss = sheet,
                sheet = "School Count by County 80 percent Undup"
    )


upc.district %>%
    filter(perc.dist >= 80) %>%
    group_by(academic_year, county_name) %>%
    summarise(n()) %>%
    sheet_write(ss = sheet,
                sheet = "District Count by County 80 percent Undup"
    )



upc.district %>%
    filter(perc.dist >= 50) %>%
    group_by(academic_year, county_name) %>%
    summarise(n()) %>%
    sheet_write(ss = sheet,
                sheet = "District Count by County 50 percent Undup"
    )
