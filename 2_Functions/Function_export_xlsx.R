## Wrapper function for xlsx, allowing the export of multiple R objects into a single Excel workbook.
## Code downloaded from: http://www.r-bloggers.com/quickly-export-multiple-r-objects-to-an-excel-workbook/

save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names=FALSE)
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE, row.names=FALSE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
