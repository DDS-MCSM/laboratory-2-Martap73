#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau Sangra Rocamora - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

if (!require(xml2)) install.packages('xml2')
library(xml2)
if (!require(tidyr)) install.packages('tidyr')
library(tidyr)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)


#compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
#cpes_filename <- "cpes.zip"
#download.file(compressed_cpes_url, cpes_filename)
#unzip(zipfile = cpes_filename)
cpe.file <- "./official-cpe-dictionary_v2.3.xml"

GetCPEItems <- function(cpe.raw) {

  #Namespaces
  xml_ns(cpe.raw)

  cpes.names <- xml2::xml_attr(xml2::xml_find_all(cpe.raw, "//d1:cpe-item"), "name")
  cpes.title<- xml2::xml_text(xml2::xml_find_all(cpe.raw, "//d1:cpe-item/d1:title[@xml:lang='en-US']"))
  cpes.cpes23<- xml2::xml_attr(xml2::xml_find_all(cpe.raw, "//d1:cpe-item/cpe-23:cpe23-item"), "name")


  # transform the list to data frame
  dataframe<-data.frame(NOMBRE=cpes.names,TITULO=cpes.title, CPES=cpes.cpes23,stringsAsFactors = F)

  # return data frame
  return (dataframe)

}

CleanCPEs <- function(cpes){

  # data manipulation

  #Separamos la columna CPES en columnas

  new.cols <- c("STD", "STD.V", "PART", "VENDOR", "PRODUCT", "VERSION", "UPDATE", "EDITION", "LANGUAGE", "SW_EDITION", "TARGET_SW", "TARGET_HW","OTHER")

  #Separador es (cualquier caracter opcional excepto \)+:
  mycpes <- tidyr::separate(data = cpes, col = CPES, into = new.cols, sep = "(?<=[^\\\\]):")

  mycpes$STD <- as.factor( mycpes$STD)
  mycpes$STD.V <- as.factor( mycpes$STD.V)

  return(mycpes)

}

Downloadfile<-function()
{
  f<-"./official-cpe-dictionary_v2.3.xml"
  if(!file.exists(f))
  {
    compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
    cpes_filename <- "cpes.zip"
    download.file(compressed_cpes_url, cpes_filename)
    unzip(zipfile = cpes_filename)
  }
  return (f)

}


ParseCPEData <- function(cpe.file) {

  # load cpes as xml file
  cpes <- xml2::read_xml(x = cpe.file)

  # get CPEs
  dfcpes <- GetCPEItems(cpes)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(dfcpes)

  # return data frame
  return(df)
}

#cpe.file<-Downloadfile()
#df<-ParseCPEData (cpe.file)


