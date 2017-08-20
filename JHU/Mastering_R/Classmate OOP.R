##loading required packages
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)

##setting basic classes
setClass("subject",
         slots=list(id="integer"))

setClass("visit",
         slots=list(visit="integer"))

setClass("room",
         slots=list(room="character"))

setClass("LongitudinalData",
         slots = list(frame="data.frame"),
         contains = list("subject", "visit", "room"))

##creating the summary class which subject, visit, room and summary methods will use to print
##idsum contains the ID being summarised, visitsum the visit being summarised, roomsum the room being summarised
##state is a flag indicating whether the data frame is data (false) or a summary (true)
setClass("LDsummary",
         slots=list(idsum="numeric",
                    visitsum="numeric",
                    roomsum="character",
                    state="logical"),
         contains = list("LongitudinalData"))

make_LD<- function(data){
     ##creating a list of unique ids
     sublist<- select(data, id)
     sublist<- as.vector(as.matrix(unique(sublist)), mode='integer')
     
     ##creating a list of unique visit identifiers
     vislist<- select(data, visit)
     vislist<- as.vector(as.matrix(unique(vislist)), mode='integer')
     
     ##creating a list of unique rooms
     roolist<- select(data, room)
     roolist<- as.vector(as.matrix(unique(roolist)), mode='character')
     
     ##creating the LongitudinalData object
     x<- new("LongitudinalData",
             id=sublist,
             visit=vislist,
             room=roolist,
             frame=data)
}

##setting the 'print' generic for the LongitudinalData object
setGeneric("print")
setMethod("print",
          "LongitudinalData",
          function(x){
               paste("Longitudinal dataset with", length(x@id), "subjects")
          })

##setting the 'print' generic for the summary object
setGeneric("print")
setMethod("print",
          signature(x ="LDsummary"),
          function(x){
               if(x@state==FALSE){
                    if(is.na(x@idsum)){
                         print(x@frame)
                    } else {
                         if(is.na(x@visitsum)){
                              cat("Subject ID:", x@idsum, "\n")
                         } else {
                              if(is.na(x@roomsum)){
                                   cat("Subject ID:", x@idsum, "\n")
                                   cat("Visit:", x@visitsum, "\n")
                              } else {
                                   cat("Subject ID:", x@idsum, "\n")
                                   cat("Visit:", x@visitsum, "\n")
                                   cat("Room:", x@roomsum, "\n")
                              }
                         }
                    }
               } else {
                    if(x@state==TRUE){
                         cat("ID:", x@idsum, "\n")
                         print(x@frame)
                    }
               }
          })

##creating the 'subject' generic method
setGeneric("subject", function(d, x){
     standardGeneric("subject")
})
setMethod("subject",
          signature("LongitudinalData", "numeric"),
          function(d, x){
               if(x %in% d@id){
                    out<-new("LDsummary", idsum=x, visitsum=NA_integer_, roomsum=NA_character_, state=FALSE, id=d@id, visit=d@visit, room=d@room, frame=filter(d@frame, id==x))
               } else {
                    out<-NULL
                    print(out)
               }
          })

##creating the 'visit' generic method
setGeneric("visit", function(d, x){
     standardGeneric("visit")
})
setMethod("visit",
          signature("LDsummary", "numeric"),
          function(d, x){
               if(x %in% d@visit){
                    out<-new("LDsummary", idsum=d@idsum, visitsum=x, roomsum=NA_character_, state=FALSE, id=d@id, visit=d@visit, room=d@room, frame=filter(d@frame, visit==x))
               } else {
                    out<-NULL
                    print(out)
               }
          })

##creating the 'room' generic method
setGeneric("room", function(d, x){
     standardGeneric("room")
})
setMethod("room",
          signature("LDsummary", "character"),
          function(d, x){
               if(x %in% d@room){
                    out<-new("LDsummary", idsum=d@idsum, visitsum=d@visitsum, roomsum=x, state=FALSE, id=d@id, visit=d@visit, room=d@room, frame=filter(d@frame, room==x))
               } else {
                    out<-NULL
                    print(out)
               }
          })

##setting the 'summary' generic method
setGeneric("summary", function(x){
     standardGeneric("summary")
})
setMethod("summary",
          signature(x="LDsummary"),
          function(x){
               if(is.na(x@roomsum)){
                    sumdat<-x@frame %>%
                         select(visit, room, value) %>%
                         group_by(visit, room) %>%
                         summarise(average = mean(value)) %>%
                         spread(room, average) %>%
                         as.data.frame()
                    out<-new("LDsummary", idsum=x@idsum, visitsum=x@visitsum, roomsum=x@roomsum, state=TRUE, id=x@id, visit=x@visit, room=x@room, frame=sumdat)
               } else {
                    sumdat<-x@frame %>%
                         select(value) %>%
                         summarize(Min = min(value),
                                   low_Qu = quantile(value, .25),
                                   Median = median(value),
                                   Mean = round(mean(value), digits = 1),
                                   hi_Qu = quantile(value, .75),
                                   Max = max(value)) %>%
                         format(nsmall = 1)
                    out<-new("LDsummary", idsum=x@idsum, visitsum=x@visitsum, roomsum=x@roomsum, state=TRUE, id=x@id, visit=x@visit, room=x@room, frame=sumdat)
               }
          })
###############################

#==========================================================
#    class to store  Longitudinal Data
#----------------------------------------------------------
#  Create an object of LongitudinalData class from dataframe
make_LD <- function(d)
{
     LD <- LongitudinalData$new(data = d)
     return(LD)
}


#  Class description
LongitudinalData <<- setRefClass("LongitudinalData",
                                 fields = list( data = "tbl_df")
)

#  generic function print()
setMethod('print', c(x = "LongitudinalData"),
          function(x, ...){
               c <- nrow(count(group_by(x$data, id)))
               print(paste("Longitudinal dataset with:",c,"subjects"));
          })


#==========================================================
# Class description of subjects
subjectClass <<- setRefClass("subjectClass",
                             fields = list( id = "numeric",
                                            data = "tbl_df"))


# generic function subject() let you get specified subject`s data from LongitudinalData
setGeneric("subject", function(x, i){
     standardGeneric("subject")
})

setMethod("subject","LongitudinalData",
          function(x, i){
               d <- filter(x$data, id == i);
               if (nrow(d) == 0 )
                    return(NULL)
               else 
               {
                    s <- subjectClass$new(id = i, data = select(d, c(visit, room,value, timepoint)))
                    return(s)
               }
          })

# summary() generic function for subjectClass 
setMethod('summary', "subjectClass",
          function(object, ...){
               object$data <- object$data %>% group_by(visit, room) %>% summarise(mean(value)) %>% spread(room, 'mean(value)') %>% ungroup()
               return (object$data)
          })

# print() generic function for subjectClass 
setMethod('print', c(x = "subjectClass"),
          function(x, ...){
               cat("Subject ID: ",x$id,"\n");
          })
#==========================================================
# visitClass - storing data related to specified 'visit'
visitClass <<- setRefClass("visitClass",
                           fields = list( id = "numeric",
                                          visit = "numeric",
                                          data = "tbl_df"))


# generic function visit() to get specified visit`s data from object of subjectClass
setGeneric("visit", function(x, v){
     standardGeneric("visit")
})

setMethod("visit","subjectClass",
          function(x, v){
               d <- filter(x$data, visit == v);
               if (nrow(d) == 0 )
                    return(NULL)
               else 
               {
                    s <- visitClass$new(id = x$id, visit = v, data = select(d, c(room, value, timepoint)))
                    return(s)
               }
          })

#==========================================================
# roomClass - intended to store data related to specified room
roomClass <<- setRefClass("roomClass",
                          fields = list( id = "numeric",
                                         visit = "numeric",
                                         room = "character",
                                         data = "tbl_df"))

setGeneric("room", function(x, r){
     standardGeneric("room")
})

# generic function room() let you get specified room`s data from object of visitClass
setMethod("room",
          c(x = "visitClass"),
          function(x, r){
               d <- filter(x$data, room == r);
               if (nrow(d) == 0 )
                    return(NULL)
               s <- roomClass$new(id = x$id, visit = x$visit, room = r, data = select(d, c(value, timepoint)))
               #d <- data.frame(cbind(as.matrix(x$value), as.matrix(x$timepoint)))
               return(s)
          })

# print() generic function for roomClass
setMethod('print', c(x = "roomClass"),
          function(x, ...){
               cat("ID: ",x$id, "\n");
               cat("Visit: ",x$visit, "\n");
               cat("Room: ",x$room, "\n");
          })

# summary() generic function for roomClass
setMethod('summary', "roomClass",
          function(object, ...){
               return(summary(object$data$value))
          })

################################

library(readr)
library(magrittr)
library(tidyr)
library(dplyr)

my.data <-read_csv("~/Dropbox/egyeb/R/_257dbf6be13177cd110e3ef91b34ff67_data/data/MIE.csv")

setClass("LongitudinalData",
         slots = list(nsubject = "numeric",
                      data = "data.frame"))
setClass("SubjectLongitudinalData",                      
         slots = list(visit = "numeric",
                      room = "character",
                      value = "numeric",
                      timepoint = "numeric"),
         contains = "LongitudinalData")

setGeneric(name="make_LD",
           def=function(n){
                standardGeneric("make_LD")
           })

setMethod("make_LD",
          c("data.frame"),
          function(n){
               new("LongitudinalData",
                   data = n,
                   nsubject=length(unique(n$id))
               )
          })
setMethod("print",
          c("LongitudinalData"),
          function(x){
               paste("Longitudinal dataset with", x@nsubject, "subjects")})

setClass("LD_filtered",
         slots = list(name = "character",
                      id_value = "numeric",
                      data = "data.frame"))

setGeneric(name="subject",
           def=function(x,y){
                standardGeneric("subject")
           }
)

setMethod("subject",
          c("LongitudinalData","numeric"),
          function(x,y){
               result <- x@data%>%filter(id == y)
               if(nrow(result) == 0){
                    return(NULL)
               }else{
                    new("LD_filtered",
                        name = "Subject ID",
                        id_value = y,
                        data = result)
               }
          })

setMethod("print",
          c("LD_filtered"),
          function(x){
               paste0(x@name, ": ", x@id_value)})

setGeneric(name="visit",
           def=function(x,y){
                standardGeneric("visit")
           }
)
setMethod("visit",
          c("LD_filtered","numeric"),
          function(x,y){
               result <- x@data%>%filter(visit == y)
               if(nrow(result) == 0){
                    return(NULL)
               }else{
                    new("LD_filtered",
                        name = "Visit",
                        id_value = y,
                        data = result)
               }
          })

setGeneric(name="room",
           def=function(x,y){
                standardGeneric("room")
           }
)

setClass("LD_room",
         slots = list(name = "character",
                      id_value = "character",
                      data = "data.frame"))
setMethod("room",
          c("LD_filtered","character"),
          function(x,y){
               result <- x@data%>%filter(room == y)
               if(nrow(result) == 0){
                    return(NULL)
               }else{
                    new("LD_room",
                        name = "Room",
                        id_value = y,
                        data = result)
               }
          })


setMethod("print",
          c("LD_room"),
          function(x){
               id <- paste("ID:",unique(x@data$id))
               visit <- paste("Visit:",unique(x@data$visit))
               room <- paste("Room:",x@id_value)
               print(id)
               print(visit)
               print(room)
          })

setGeneric(name="summary",
           def=function(x){
                standardGeneric("summary")
           }
)

setMethod("summary",
          c("LD_filtered"),
          function(x){
               res <- x@data %>% 
                    group_by(visit,room) %>% 
                    summarize(pollen = mean(value)) %>% 
                    spread(room,pollen)
               list(paste0(x@name, ": ", x@id_value),
                    res)
          })

setMethod("summary",
          c("LD_room"),
          function(x){
               list(paste0("ID: ", unique(x@data$id)),
                    base::summary(x@data$value))
          })

#testing
gizi <- make_LD(my.data)
print(gizi)

out <- subject(gizi, 14)
print(out)
print(subject(gizi, 10))

out <- subject(gizi, 44) %>% visit(0)%>%room("bedroom")
print(out)

out <- subject(gizi, 54) %>% summary
print(out)

out <- subject(gizi, 14) %>% summary
print(out)

out <- subject(gizi, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(gizi, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
