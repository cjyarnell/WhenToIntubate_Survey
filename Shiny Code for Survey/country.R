# Language-specific country code

country_by_language <- function(lk){

  countries_df <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
  countries <- tolower(countries_df[,"alpha.2"])
  countries2 <- countries_df[,"name"]
  
    if (lk == "English"){
    
    country_order <- c(11, # Argentina
                       14, # Australia
                       32, # Brazil
                       41, # Canada 
                       46, # China
                       77, # France
                       84, # Germany
                       111, # Italy
                       113, # Japan
                       201, # Singapore
                       221, # Thailand
                       235, # UK
                       236, # USA
                       1:10,
                       12:13,
                       15:31,
                       33:40,
                       42:45,
                       47:76,
                       78:83,
                       85:110,
                       112:200,
                       202:220,
                       222:234,
                       237:249)
    
    default <- "Canada"
    
  } else if (lk == "French"){
    
    country_order <- c(41, # Canada 
                       77, # France
                       1:40,
                       42:76,
                       78:249)

    default <- "France"
    
  } else if (lk == "Spanish"){

    espanol_countries <- c(  "Argentina"                                   
                             , "Belize"                                      
                             , "Bolivia (Plurinational State of)"            
                             , "Chile"                                       
                             , "Colombia"                                    
                             , "Costa Rica"                                  
                             , "Ecuador"                                     
                             , "El Salvador"                                 
                             , "Guatemala"                                   
                             , "Honduras"                                    
                             , "Mexico"                                      
                             , "Nicaragua"                                   
                             , "Panama"                                      
                             , "Paraguay"                                    
                             , "Peru"                                        
                             , "Spain"                                   
                             , "Uruguay"                                     
                             , "Venezuela (Bolivarian Republic of)")
    country_order <- c(which(countries2 %in% espanol_countries),
                       which(!(countries2 %in% espanol_countries)))
    
    default <- "Argentina"
        
  } else if (lk == "Italian"){
    
    country_order <- c(1:249)
    
    default <- "Italy"
    
  } else if (lk == "German"){
    
    country_order <- c(1:249)
    
    default <- "Germany"
    
  } else if (lk == "Portuguese"){
    
    country_order <- c(32, # Brazil
                       182,
                       1:31,
                       33:181,
                       183:249)
    
    default <- "Brazil"
    
  } else if (lk == "Japanese"){
    
    country_order <- c(113, # Japan
                       1:112,
                       114:249)
    
    default <- "Japan"
    
  } else if (lk == "Chinese"){
    
    country_order <- c(46, # China
                       1:45,
                       47:249)
    
    default <- "China"
    
  } else if (lk == "Thai"){
    
    country_order <- c(1:249)
    
    default <- "Thailand"
    
  } else if (lk == "Indonesian"){
    
    country_order <- c(1:249)
    
    default <- "Indonesia"

      } else {print("error")}
  
  countries2 <- countries2[country_order]
  
  return(list(countries = countries2, default = default))
  
}
