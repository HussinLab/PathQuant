
# Function fractioning reaction list from every gene from KGML file
#
#
# exemple : id R1... R2... R3... ko
#  -> id R1 ko
#  -> id R2 ko
#  -> id R3 ko
#
#  @param data frame des reactions extraites du KGML en listes
#  @keywords  kegg
#  @examples completeEdgeList(edgeDataFrame)

unlistEdgeReactionCol <- function(edgeDataFrame){


    #print("unlistEdgeReactionCol ")
    s <- strsplit(as.vector(edgeDataFrame$reactions), " ");
    edgeDataFrame <- data.frame(
        kgmlId = rep(edgeDataFrame$kgmlId, sapply(s, length)),
        reactions = unlist(s),
        type = rep(edgeDataFrame$type, sapply(s, length)),
        ko = rep(edgeDataFrame$ko, sapply(s, length)))

    return <- edgeDataFrame;


}


correctReactionString <- function(edgeDF){

    #  print("correctReactionString")

    edgeDF <- lapply(edgeDF,
                     function (x) gsub("rn:","",x))

    return <- edgeDF;

}

# Since it is not possible to have the related gene when parsing the
# reaction nodes in the KGML. I had to parse de reaction nodes and the
# gene entry nodes separatly and then combinde the 2 with the information
# needed.
#
# the results gives a data.frame(substrateId, productId, subtrateName,
# productName, reactionId, reactionName, ko(gene))



finalReactionEdgeDF <- function(computer_path_to_map_folder,pathwayId){

    #print("finalReactionEdgeDF")
    #Debug print
    #print(pathwayId)
    edgeDF <- getListEdgeFromGeneKGML(computer_path_to_map_folder,pathwayId);

    reactionDF <- getListReactionFromKGML(computer_path_to_map_folder,pathwayId);

    if(!nrow(reactionDF) == 0 && !nrow(edgeDF) == 0){
        reactionDF <- as.data.frame(lapply(reactionDF,
                                           function(X) unname(unlist(X))));

        mergeDF <-  merge(reactionDF, edgeDF, by="reactionId");

        mergeDF <- mergeDF[ -c(8,9)];
        mergeDF <- mergeDF[ c(2,3,4,5,1,6,7,8)];

    }else{
        mergeDF <- NULL;

    }

    return <- mergeDF;

}



