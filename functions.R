GetDataFromState <- function() {
	page <- readLines("https://www.mass.gov/info-details/archive-of-covid-19-cases-in-massachusetts#december-2021-")
	focal_line <- page[grepl("/doc/covid-19-raw-data",page)][1]
	url <- paste0("https://www.mass.gov",gsub('.{1}$', "", gsub('href=\"',"",stringr::str_extract(focal_line, pattern="href=[^>]*"))))
	
	temp = tempfile(fileext = ".xlsx")
 	download.file(url, destfile=temp, mode='wb')

    state_data <- readxl::read_xlsx(temp, sheet=26)
	medford_data <- subset(state_data, `City/Town`=="Medford")
	medford_data$Start_Date <- as.Date(medford_data$Start_Date)
	medford_data$End_Date <- as.Date(medford_data$End_Date)
	medford_data$Report_Date <- as.Date(medford_data$`Report Date`)
	medford_data$Percent_Positivity <- 100*as.numeric(gsub('%', "", medford_data$`Percent Positivity`))
	medford_data$Daily_Rate_Per_100K_Residents <- as.numeric(medford_data$`Average Daily Rate`)
	return(medford_data)
}

GetDataFromCity <- function() {
	input_file_html <- rvest::read_html("https://www.medfordma.org/covid-19-total-case-counts/")
	tbl <- as.data.frame(rvest::html_table(rvest::html_nodes(input_file_html, "table"), header=1)[[1]])
	current_year <- format(Sys.Date(), "%Y")
	for (i in sequence(nrow(tbl))) {
		if(grepl('\\*', tbl[i,1])) {
			current_year <- as.numeric(gsub('\\*', "", tbl[i,1]))-1
		} else {
			tbl[i,1] <- paste0(tbl[i,1], "-", current_year)
		}
	}
	tbl <- tbl[!grepl('\\*', tbl[,1]),]
	tbl$Date <- as.Date(tbl$Date, format="%m-%d-%Y")
	tbl$`TOTAL CASES TO DATE` <- as.numeric(gsub(',', "", tbl$`TOTAL CASES TO DATE`))
	return(tbl)
}




FocalCountiesHospitalKnoxArea <- function() {
    return(c("Knox", "Anderson", "Roane", "Scott", "Blount", "Claiborne", "Jefferson", "Campbell", "Sevier", "Loudon", "Hamblen", "Cocke", "Monroe", "McMinn"))
}

FocalCountiesEastTN <- function() {
    return(c("Anderson", "Bledsoe", "Blount", "Bradley", "Campbell", "Carter", "Claiborne", "Cocke", "Cumberland", "Grainger", "Greene", "Hamblen", "Hamilton", "Hancock", "Hawkins", "Jefferson", "Johnson", "Knox", "Loudon", "Marion", "McMinn", "Meigs", "Monroe", "Morgan", "Polk", "Rhea", "Roane", "Scott", "Sevier", "Sullivan", "Unicoi", "Union", "Washington"))
}








CreateHHSData <- function() {

    # hhs_sources <- jsonlite::fromJSON("https://healthdata.gov/data.json?page=0")
    # capacity_by_facility_number <- grep("COVID-19 Reported Patient Impact and Hospital Capacity by Facility", hhs_sources$dataset$title)


    # capacity_by_facility_url <- hhs_sources$dataset$distribution[capacity_by_facility_number][[1]]$downloadURL[1] #often a week behind though
    # temp = tempfile(fileext = ".csv")

    # utils::download.file(capacity_by_facility_url, temp, method="libcurl")

    # hhs_capacity <- read.csv(file=temp)
	
	temp = tempfile(fileext = ".csv")
 	dataURL <- "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD&api_foundry=true"
 	download.file(dataURL, destfile=temp, mode='wb')

 	hhs_capacity <- read.csv(temp, header=TRUE)
    hhs_capacity_ma <- subset(hhs_capacity, state=="MA")
	hhs_capacity_ma[hhs_capacity_ma==-999999] <- NA
	hhs_capacity_ma[hhs_capacity_ma=="-999999"] <- NA

    hhs_capacity_ma$percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied <- 100 * hhs_capacity_ma$total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg / hhs_capacity_ma$all_adult_hospital_inpatient_bed_occupied_7_day_avg

    hhs_capacity_ma$percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds <- 100 * hhs_capacity_ma$all_adult_hospital_inpatient_bed_occupied_7_day_avg / hhs_capacity_ma$all_adult_hospital_inpatient_beds_7_day_avg

    hhs_capacity_ma$percentage_adult_hospital_inpatient_bed_unoccupied_of_all_inpatient_beds <- 100 - hhs_capacity_ma$percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds

    hhs_capacity_ma$number_unoccupied_adult_hospital_inpatient_beds <- hhs_capacity_ma$all_adult_hospital_inpatient_beds_7_day_avg - hhs_capacity_ma$all_adult_hospital_inpatient_bed_occupied_7_day_avg



    hhs_capacity_ma$percentage_adult_hospital_ICU_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_ICU_occupied <- 100 * hhs_capacity_ma$staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg / hhs_capacity_ma$staffed_adult_icu_bed_occupancy_7_day_avg

    hhs_capacity_ma$percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds <- 100 * hhs_capacity_ma$staffed_adult_icu_bed_occupancy_7_day_avg / hhs_capacity_ma$total_staffed_adult_icu_beds_7_day_avg

    hhs_capacity_ma$percentage_adult_hospital_inpatient_ICU_bed_unoccupied_of_all_inpatient_ICU_beds <- 100 - hhs_capacity_ma$percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds

    hhs_capacity_ma$number_unoccupied_adult_hospital_ICU_beds <- hhs_capacity_ma$total_staffed_adult_icu_beds_7_day_avg - hhs_capacity_ma$staffed_adult_icu_bed_occupancy_7_day_avg
    return(hhs_capacity_ma)
}


CreateHHSDataFocalCities <- function(hhs_capacity_ma) {

    focal_cities <- toupper(c("Boston", "Burlington", "Medford", "Melrose", "Winchester", "Cambridge", "Salem"))
    hhs_capacity_ma_focal <- hhs_capacity_ma[hhs_capacity_ma$city%in%focal_cities,]

    hhs_capacity_ma_focal <- subset(hhs_capacity_ma_focal, 
        !is.na(all_adult_hospital_inpatient_bed_occupied_7_day_avg) & 
        !is.na(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg) & 
        !is.na(all_adult_hospital_inpatient_beds_7_day_avg) &
        !is.na(total_staffed_adult_icu_beds_7_day_avg) &
        !is.na(staffed_adult_icu_bed_occupancy_7_day_avg) &
        !is.na(staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg) & 
        !is.na(percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied) &
        !is.na(percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds) &
        !is.na(percentage_adult_hospital_ICU_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_ICU_occupied) &
        !is.na(percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds)
    )


    hhs_capacity_ma_focal <- subset(hhs_capacity_ma_focal, 
        (all_adult_hospital_inpatient_bed_occupied_7_day_avg >= 0) & 
        (total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg >= 0) & 
        (all_adult_hospital_inpatient_beds_7_day_avg >= 0) &
        (total_staffed_adult_icu_beds_7_day_avg >= 0) &
        (staffed_adult_icu_bed_occupancy_7_day_avg >= 0) &
        (staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg >= 0)
    )

    #hhs_capacity_ma_focal <- subset(hhs_capacity_ma_focal, all_adult_hospital_inpatient_beds_7_day_avg>=100)

    few_update_hospitals <- names(which(table(hhs_capacity_ma_focal$hospital_name)<=2))

    if(length(few_update_hospitals)>0) {
        hhs_capacity_ma_focal <- subset(hhs_capacity_ma_focal, !(hospital_name %in% few_update_hospitals)) #deleting the tiny hospitals that don't update
    }

    hhs_capacity_ma_focal$hospital_name <- gsub("TENNOVA HEALTHCARE", "TENNOVA", hhs_capacity_ma_focal$hospital_name )
    hhs_capacity_ma_focal$hospital_name <- stringr::str_to_title(hhs_capacity_ma_focal$hospital_name)
   # hhs_capacity_ma_focal$hospital_name[grepl("University Of Tn", hhs_capacity_ma_focal$hospital_name, ignore.case=TRUE)] <- "University of TN Medical Center"




    hhs_capacity_ma_focal$DATE <- as.Date(hhs_capacity_ma_focal$collection_week)

    # hhs_capacity_ma_focal_cities <- hhs_capacity_ma_focal

    # hhs_capacity_ma_focal_cities <- hhs_capacity_ma_focal %>% group_by(city, DATE) %>% summarize_at(vars(number_unoccupied_adult_hospital_inpatient_beds, number_unoccupied_adult_hospital_ICU_beds), list(sum = ~sum(., na.rm=TRUE)))

    # combinations <- expand.grid(DATE = unique(hhs_capacity_ma_focal_cities$DATE), city = unique(hhs_capacity_ma_focal_cities$city))

    # hhs_capacity_ma_focal_cities <- full_join(hhs_capacity_ma_focal_cities, combinations, by = c("DATE" = "DATE", "city" = "city")) %>% mutate(number_unoccupied_adult_hospital_inpatient_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_inpatient_beds_sum), 0, number_unoccupied_adult_hospital_inpatient_beds_sum)) %>% mutate(number_unoccupied_adult_hospital_ICU_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_ICU_beds_sum), 0, number_unoccupied_adult_hospital_ICU_beds_sum))

    # hhs_capacity_ma_focal_cities$city <- stringr::str_to_title(hhs_capacity_ma_focal_cities$city)

    return(hhs_capacity_ma_focal)
}

CreateHHSDataFocalCitiesPretty <- function(hhs_capacity_ma_focal) {
	
    hhs_capacity_ma_focal_latest <- subset(hhs_capacity_ma_focal, DATE==max(DATE))
    hhs_capacity_ma_focal_latest <- hhs_capacity_ma_focal_latest[order(hhs_capacity_ma_focal_latest$all_adult_hospital_inpatient_beds_7_day_avg, decreasing=TRUE),]
    hhs_capacity_ma_focal_latest_pretty <- hhs_capacity_ma_focal_latest[,c(
        "hospital_name", 
        "city", 
        "all_adult_hospital_inpatient_beds_7_day_avg", 
        "number_unoccupied_adult_hospital_inpatient_beds", 
        "percentage_adult_hospital_inpatient_bed_unoccupied_of_all_inpatient_beds", 
        "total_staffed_adult_icu_beds_7_day_avg", 
        "number_unoccupied_adult_hospital_ICU_beds", 
        "percentage_adult_hospital_inpatient_ICU_bed_unoccupied_of_all_inpatient_ICU_beds"
    )]
    colnames(hhs_capacity_ma_focal_latest_pretty) <- c(
        "Hospital", 
        "City", 
        "Adult beds total", 
        "Adult beds number avail", 
        "Adult beds % avail", 
        "Adult ICU total", 
        "Adult ICU number avail", 
        "Adult ICU % avail"
    )

    for (i in 3:ncol(hhs_capacity_ma_focal_latest_pretty)) {
        hhs_capacity_ma_focal_latest_pretty[,i]<- round(hhs_capacity_ma_focal_latest_pretty[,i])
    }

    hhs_capacity_ma_focal_latest_pretty$City <- stringr::str_to_title(hhs_capacity_ma_focal_latest_pretty$City)
    rownames(hhs_capacity_ma_focal_latest_pretty) <- NULL

    return(hhs_capacity_ma_focal_latest_pretty)
}




target_save_csv <- function(x, filename) {
	write.csv(x, file=filename, row.names=FALSE)	
	return(filename)
}

GetTSAThroughput <- function() {
	tsa <- read_html("https://www.tsa.gov/coronavirus/passenger-throughput")
	tsa_screening <- tsa %>% html_elements("table") %>% html_table()
	tsa_screening <- tsa_screening[[1]]
	tsa_screening$Day <- format(as.Date(tsa_screening$Date, format="%m/%d/%Y"), "%m/%d")
	tsa_summary <- data.frame(Date=as.Date(tsa_screening$Date, format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2021 Traveler Throughput`)))
	tsa_summary <- rbind(tsa_summary, data.frame(Date=as.Date(paste0(tsa_screening$Day, "/2020"), format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2020 Traveler Throughput`))))
	tsa_summary <- rbind(tsa_summary, data.frame(Date=as.Date(paste0(tsa_screening$Day, "/2019"), format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2019 Traveler Throughput`))))
	tsa_summary <- tsa_summary[order(tsa_summary$Date),]	
	return(tsa_summary)
}

GetBOSFlights <- function() {
	#flights <- read.csv("~/Dropbox/Flight/208423505_T_DB1B_COUPON.csv")	
	flight_files <- list.files("~/Dropbox/Flight/", pattern=".*.csv")
	bos_flights <- data.frame()
	for (i in seq_along(flight_files)) {
		flights <- read.table(paste0("~/Dropbox/Flight/", flight_files[i]), header=TRUE, sep=",")
		bos_flights <- rbind(bos_flights, flights)
	}
	#print(sort(unique(paste0(bos_flights$YEAR, "_",bos_flights$MONTH))))
	bos_flights_only <- rbind(subset(bos_flights, ORIGIN=="BOS"), subset(bos_flights, DEST=="BOS"))
	return(bos_flights_only)
}
