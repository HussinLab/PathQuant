
# A KEGG Function
#
# This function allows you to get a KGML format of a pathway and save
# xml format in data directory
# getPathwayKGML("hsa01100")

getPathwayKGML <- function(pathwayId) {

    #cat("getPathwayKGML  \n")

    adressfile <- toStringAdressfile(pathwayId)
    
    #cat(destfile)

    URL_S <- "https://rest.kegg.jp/get/"
    URL_E <- "/kgml"

    URL_FINAL<- paste(URL_S,pathwayId, URL_E, sep="");

    #cat(URL_FINAL)
    xmlFile <- RCurl::getURL(URL_FINAL);

    if (xmlFile == "") {
        stop(paste0("Pathway querry '",pathwayId, "' does not exist in KEGG database."), call. = FALSE)
    } else{
        xmlFile <- XML::xmlParse(xmlFile)


        date <- as.character(format(Sys.time(),"%Y_%m_%d"))
        #cat(date)

        path_download_maps <- file.path(here::here("Maps",paste0("Version_Downloaded_",date,"/")))

        if(!file.exists(path_download_maps)) {
            dir.create(path_download_maps,showWarnings = T,recursive = T,mode = "0777")
            message(paste0("Directory ",path_download_maps," has been created will try to download the queried maps."))
        }

        XML::saveXML(xmlFile, file = paste0(path_download_maps,pathwayId,".txt"))
        message("Queried maps are downloaded and located at this path : ",paste0(here::here("Maps",paste0("Version_Downloaded_",date,"/"))))

    }

    return <- xmlFile

}

#set path to store downloaded file
toStringDestfile <- function(pathwayId,computer_path_to_map_folder){
    #concatenation of pathwayId to set swdir for the xml

    #cat("toStringDestfile  \n")
    #cat("what do u mean no default",computer_path_to_map_folder)
    s2 <-  toString(pathwayId);
    s3 <- ".txt"

    destfile <- paste0(computer_path_to_map_folder,s2, s3, sep="");

    return <- destfile;
}

#set path for download
toStringAdressfile <- function(pathwayId){

    #cat("toStringAdressfile  \n")

    s1 <- "rest.kegg.jp/get/";
    s2 <-  toString(pathwayId);
    s3 <- "/kgml"
    s4 <- paste(s1,s2, sep= "");
    adressfile <- paste(s4, s3, sep="");

    return <- adressfile;
}


# see if file was already dowloaded
isFileInDirectory <- function(pathwayId,computer_path_to_map_folder){
    #concatenation of pathwayId to set swdir for the xml
    #cat("isFileInDirectory \n")

    pathFile <- toStringDestfile(pathwayId,computer_path_to_map_folder)

    bool <- FALSE;
    res <-  tryCatch({
        XML::xmlParse(pathFile)
        bool <- TRUE
    }, error = function(e) {
        bool <- FALSE;
    }, finally = {
        return <- bool;
    })

    return <- res;
}

# set adress to download compound kgml file
toCompoundAdressfile <- function(compoundKeggId){

    #cat("toCompoundAdressfile  \n")

    s1 <- "rest.kegg.jp/list/";
    s2 <-  toString(compoundKeggId);

    adressfile <- paste(s1,s2, sep= "");

    return <- adressfile;
}

get.url.list.pathway.by.organism <- function(organism_code){


    url_Address <-  "https://rest.kegg.jp/list/pathway/hsa";
    adressfile <- paste(url_Address,organism_code, sep= "");

    return <- url_Address;

}


downloadFileByUrl <- function(url){

    file <- RCurl::getURL(url);


    return <- file;

}
