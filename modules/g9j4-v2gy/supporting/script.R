if(!require(readxl)) install.packages('readxl')
if(!require(dplyr)) install.packages('dplyr')

#####################################################
# Make sure to unzip nonidentifying_data.zip first! #
#####################################################

# Get the rows/columns of the information (created on a previous run)
infoDat <- read.csv('summary-rows-cols.csv')
# Select out the ones without rows/columns
infoSel <- infoDat[!infoDat$rows == 0 | !infoDat$columns == 0,]
rowsP75 <- summary(infoSel$rows)[5]
columnsP75 <- summary(infoSel$columns)[5]

# Get all file names of relevance
folder <- 'nonidentifying_data'
# Ensure there is no previous `principal-dataset.csv` already there!
# This script will only create a new file if there is none, otherwise it'll append.
principalFileName <- 'principal-dataset.csv'
filesLocal <- list.files(folder)
filesSelect <- filesLocal[grepl(x = filesLocal, pattern = "*.(xls|xlsx|csv|dat|txt)")]

# Scaffold objects to save to
principalFile <- data.frame()
fileName <- c()
rows <- c()
columns <- c()

# Get the summary information on each datafile and sheet
for (file in filesSelect) {
    if (grepl(file, pattern = "*.(xls|xlsx)")) {
        sheetsFile <- excel_sheets(sprintf('%s/%s', folder, file))
        for (sheet in sheetsFile) {
            fileData <- read_excel(sprintf('%s/%s', folder, file), sheet, col_names = FALSE)

            # Collect summary-rows-cols
            fileName <- c(fileName, sprintf('%s_%s', file, sheet))
            rows <- c(rows, dim(fileData)[1])
            columns <- c(columns, dim(fileData)[2])
            
            # Sample data from file
            write.table(fileData[sample(1:dim(fileData)[1], size = min(dim(fileData)[1], rowsP75)), sample(1:dim(fileData)[2], size = min(dim(fileData)[2], columnsP75))], principalFileName, append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)
        }
    } else if (grepl(file, pattern = "*.csv")) {
        fileData <- read.csv(sprintf('%s/%s', folder, file))

        # Collect summary-rows-cols
        fileName <- c(fileName, sprintf('%s', file))
        rows <- c(rows, dim(fileData)[1])
        columns <- c(columns, dim(fileData)[2])
        
        # Sample data from file
        write.table(fileData[sample(1:dim(fileData)[1], size = min(dim(fileData)[1], rowsP75)), sample(1:dim(fileData)[2], size = min(dim(fileData)[2], columnsP75))], principalFileName, append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)
    } else if (grepl(file, pattern = "*.dat")) {
        fileData <- read.table(sprintf('%s/%s', folder, file))

        # Collect summary-rows-cols
        fileName <- c(fileName, sprintf('%s', file))
        rows <- c(rows, dim(fileData)[1])
        columns <- c(columns, dim(fileData)[2])

        # Sample data from file
        write.table(fileData[sample(1:dim(fileData)[1], size = min(dim(fileData)[1], rowsP75)), sample(1:dim(fileData)[2], size = min(dim(fileData)[2], columnsP75))], principalFileName, append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)
    } else if (grepl(file, pattern = "*.txt")) {
        fileData <- read.table(sprintf('%s/%s', folder, file), sep = ifelse(grepl(x = file, pattern = "^(4|13|63)"), '\t', ','), fileEncoding = ifelse(file == "13.txt", "utf-16le", ""))
       
        # Collect summary-rows-cols
        fileName <- c(fileName, sprintf('%s', file))
        rows <- c(rows, dim(fileData)[1])
        columns <- c(columns, dim(fileData)[2])

        # Sample data from file
        write.table(fileData[sample(1:dim(fileData)[1], size = min(dim(fileData)[1], rowsP75)), sample(1:dim(fileData)[2], size = min(dim(fileData)[2], columnsP75))], principalFileName, append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)
    }
}

# You could uncomment this to create the summary-rows-cols in the parent folder
# write.csv(data.frame(fileName, rows, columns), 'summary-rows-cols.csv', row.names = FALSE)