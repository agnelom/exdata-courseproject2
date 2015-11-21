plot1 <- function(){
    
    ## Define the Path     
    fPath <- file.path("exdata-data-NEI_data")
    
    ## Load the data from .rds files 
    NEI <- readRDS(file.path(fPath,"summarySCC_PM25.rds"))
    SCC <- readRDS(file.path(fPath,"Source_Classification_Code.rds"))
    
    ## Group NEI data by year
    NEIGrp <- group_by(NEI,year)
    ## Summerizing the the grouped data to get sum totals by year
    yearTots <- summarise(NEIGrp,Emissions=sum(Emissions)) 
    ## Dividing Emissions amount by 1000000, in order to represent it in million tons on the chart
    yearTots <- mutate(yearTots, Emissions = Emissions/1000000)

    png("plot1.png",height = 480, width = 480,bg="transparent")
        barplot(yearTots$Emissions, 
                names.arg = yearTots$year,
                main = "Total Emissions for PM2.5 in US (1999 - 2008)",
                xlab = "Year",
                ylab = "PM2.5 Emissions (million tons)",
                col = "dark green"
                )
    dev.off()
}

