plot2 <- function(){
    
    ## Define the Path     
    fPath <- file.path("exdata-data-NEI_data")
    
    ## Load the data from .rds files 
    NEI <- readRDS(file.path(fPath,"summarySCC_PM25.rds"))
    SCC <- readRDS(file.path(fPath,"Source_Classification_Code.rds"))
    
    ## Retrieve all records for fips=24510, I am going to use the filter command of the dplyr package
    ## But will first group the data from NEI by year
    NEIGrp <- group_by(NEI,year)
    NEIfips24510 <- filter(NEIGrp, fips==24510)
    
    ## Summerizing the the grouped subset data for fips=24510 to get sum totals by year for Baltimore city
    yearTots <- summarise(NEIfips24510,Emissions=sum(Emissions)) 

    png("plot2.png",height = 480, width = 480,bg="transparent")
    barplot(yearTots$Emissions, 
            names.arg = yearTots$year,
            main = "PM2.5 Emissions in Baltimore City, Maryland (1999 - 2008)",
            xlab = "Year",
            ylab = "PM2.5 Emissions (tons)",
            col = "dark green"
    )
    dev.off()
}

