#' Convert AMLR location strings to accepted values
#'
#' Convert AMLR location strings to accepted values
#'
#' @param x data.frame, must contain character column 'location'
#'
#' @details
#' AMLR (pinniped) data is frequently messy, including having non-standard
#' location information (location names). This function is a 'repository'
#' of approved conversions to standardize location names
#' in an ***REMOVED*** dataset.
#' This function was built for the tag resight reimport effort,
#' but is applicable to other location renaming as well
#'
#' Input data are processed as followed before being matched:
#'   - '.' characters are removed (i.e., replaced with '')
#'   - Non-ASCII text is escaped using
#'     \code{\link[stringi:stri_escape_unicode]{stri_escape_unicode}}
#'
#' @return
#' A data frame, with an updated 'locations' column
#' but otherwise the same columns and types.
#'
#' @examples
#' x <- data.frame(id = 1:3, location = c("P Bahamonde", "Bahamonde", "Chung"))
#' mutate_location(x)
#' # OR
#' \dontrun{x %>% mutate_location()}
#'
#' @export
mutate_location <- function(x) {
  if (!("location" %in% names(x) & inherits(x$location, "character")))
    stop("The data frame must have a column location of type character")

  x %>%
    mutate(loc_clean = str_replace_all(stri_escape_unicode(location), "[.]", ""),
           loc_lower = tolower(loc_clean),
           # https://stackoverflow.com/questions/22223431
           # stringi::stri_escape_unicode(c("á", "í", "ú"))
           location =
             case_when(
               loc_lower %in% tolower(c("AH pt")) ~ "Punta Odontoceto",
               loc_lower %in% tolower(c("Playa Alcazar", "Alcazor", "Alcazar flats", "Planicie Alcazar", "PAlcazar", "P Alcazar")) ~ "Alcazar",
               loc_lower %in% tolower(c("Angosta", "Pl Angosta", "P Angosta")) ~ "Angosta",
               # loc_lower %in% tolower(c("Antarctica", "Playa Antarctica", "Pl Antarctica", "Antártico", "P Antarctico", " P Antarctica", "P Antarctica", "Antartica", "Antarctico")) ~ "Antartico",
               loc_lower %in% tolower(c("Antarctica", "Playa Antarctica", "Pl Antarctica", paste0("Ant", "\\u00e1", "rtico"), "P Antarctico", " P Antarctica", "P Antarctica", "Antartica", "Antarctico")) ~ "Antartico",

               loc_lower %in% tolower(c(" Amarya", "Amarya", "behind Elmira", "Elmira", "Aymara F")) ~ "Aymara",
               loc_lower %in% tolower(c("P Bahamonde", "Bahamunde", "B'monde/La Cav", "B Mon", "Bajamonde", "Baha Monde", "B'monde")) ~ "Bahamonde",

               loc_lower %in% tolower(c("Ballena hill", "Ballena hills", "Ballena H")) ~ "Ballena Hill",
               loc_lower %in% tolower(c("Ballena", "Ballena N", "Ballena THrte", "B Nor", "Ballena Norte", "BNorte/Sur", "B Norte", "BNorte", "Plan Ball Norte", "Ballena C", "BN")) ~ "Ballena Norte",
               loc_lower %in% tolower(c("Ballena flats", "Ball Fl", "Ballena Fl", "B Norte flats", "BNorte Flats", "Ballena N Flats", "Ballena Centra Flats", "Flats Ballenas", "Flats Ballena Norte", "Planicie Ballena norte", "Ballena F", "Ballena W", "Ballena")) ~ "Ballena Flats",
               loc_lower %in% tolower(c("Ballena S", "Ballend S", "Ball S", "BSur", "BSur ridge", "B Sur", "ballena sur", "Pla BallSur", "Planicie Ballena Sur", "Ballend/Chag")) ~ "Ballena Sur",
               loc_lower %in% tolower(c("Plan Ball Sur", "B Sur flats", "Plan Ballena Sur", "BSur flats")) ~ "Ballena Sur Flats",

               loc_lower %in% tolower(c("Cach", "Cach slope", "Cacheros", "Cacharros", "Cach/Chung", "Cachomos", "Mad/Cach")) ~ "Cachorros",
               loc_lower %in% tolower(c("Cach (E)", "Cach(E)", "Cach-E", "Cach E", "Cach C/E", "Cachorros E", "E Cach")) ~ "Cachorros East",
               loc_lower %in% tolower(c("Cach-C", "Cach C")) ~ "Cachorros Central",
               loc_lower %in% tolower(c("Cach-W", "Cach W")) ~ "Cachorros West",
               loc_lower %in% tolower(c("Cach-flats", "Cach flats", "Cach F", "Planicie Cachorro", "W Cach Flats", "W Cach Flats", "Flats Cachorros", "Cacharros Flats")) ~ "Cachorros Flats",
               loc_lower %in% tolower(c("Cach-ridge", "Cach C ridge", "Cach  ridge", "Cach Ridge", "Cach R", "Cacheros Ridge")) ~ "Cachorros Ridge",
               loc_lower %in% tolower(c("Cach H", "Cach Hill")) ~ "Cachorros Hill",

               loc_lower %in% tolower(c("Chugungo", "Chango", "Chung high", "Chung", "Chun", "Chungungu", "Chungunga", "Chung/Ballena")) ~ "Chungungo",
               loc_lower %in% tolower(c("E Chungungo", "Chung E high", "Chung (E)", "Chung(E)", "Chung-E", "Chung E", "Chungungo east", "Chin E", "Chagunc? Chag E?")) ~ "Chungungo East",
               loc_lower %in% tolower(c("Chung-C", "Chu C", " Chung C/W", "Chung C", "Chungungo C", "Chun C")) ~ "Chungungo Central",
               loc_lower %in% tolower(c("Chungungo W", "Chung W", "Chung-W")) ~ "Chungungo West",
               loc_lower %in% tolower(c("Chung Flats", "Chung Flat", "Chun Flats", "Chung Fl", "Chung flat C", "Plan Chungungo", "Chung F", "Chung F C", "Chung C flat", "Chungunga Flat", "Chung Fts C", "chungungo flats", "chung-flat", "Flats Chungungo", "Flats Chungungo", "Planicie Chungungo", "Chung flats C", "Ch Flats")) ~ "Chungungo Flats",
               loc_lower %in% tolower(c("Chung Flats E", "Chu F E", "Chung F E", "Chung E Flats", "Chung flat E", "Chung E flat")) ~ "Chungungo East Flats",
               loc_lower %in% tolower(c("Chung Flats W", "Chung flat W", "Chung f W", "Chung W flat")) ~ "Chungungo West Flats",
               loc_lower %in% tolower(c("Chung R", "Chung W ridge", "Chung ridge")) ~ "Chungungo Ridge",
               loc_lower %in% tolower(c("Chung-hill", "Chung W hill", "Chung F C hill", "Chung F hill", "Chung cliff", "Cerro Chungungo", "Chung hill", "Chungungo hill", "Cerro Chung", "Co Chungungo", "Chung H")) ~ "Chungungo Hill",

               loc_lower %in% tolower(c("CopiHue High", "Copi Hue")) ~ "Copihue",
               loc_lower %in% tolower(c("Cop", "Copi", "Copi high", "Copi C Knoll", "Copi S", "Plan Copi-Maderas")) ~ "Copi",
               loc_lower %in% tolower(c("Copi-E", "Copi E")) ~ "Copi East",
               loc_lower %in% tolower(c("Copi-C", "Copi C", "Copi C/E", "Copi C high", "Copi-C/W")) ~ "Copi Central",
               loc_lower %in% tolower(c("Cop-W", "Copi-W", "Copi W")) ~ "Copi West",
               loc_lower %in% tolower(c("Cop Flats", "Copi flats", "Copi Fl", "Copi F", "Copi Flat", "Copi-flats", "Copihue slope", "Plan Copihue", "Plan Vero", "Planicie Copihue", "Flats Copihue", "Flats Copihue", "Plan Copi", "Cop F", "Copihue Flats", "Copihue plano")) ~ "Copi Flats",
               loc_lower %in% tolower(c("Copi Flats E", "Copi E Flats")) ~ "Copi Flats East",
               loc_lower %in% tolower(c("Copi Flats W", "Copi W Flats")) ~ "Copi Flats West",
               loc_lower %in% tolower(c("Copi hill", "copi-hill", "Copi H", "Hue H")) ~ "Copi Hill",
               loc_lower %in% tolower(c("Copi Saddle", "Copi ridge", "Copi R", "Hue Ridge")) ~ "Copi Ridge",

               loc_lower %in% tolower(c("Co Delfin")) ~ "Delfin",

               loc_lower %in% tolower(c("Hue", "Hue wE")) ~ "Hue",
               loc_lower %in% tolower(c("Hue-E", "Hue E")) ~ "Hue East",
               loc_lower %in% tolower(c("Hue-C", "Hue C", "Hue C high", "Hue C/W", "Huey C")) ~ "Hue Central",
               loc_lower %in% tolower(c("Hue-W", "Hue W")) ~ "Hue West",
               loc_lower %in% tolower(c("Hue Far E")) ~ "Hue Far East",
               loc_lower %in% tolower(c("Hue S")) ~ "Hue South",

               loc_lower %in% tolower(c("Playa Lobero", "Lobero")) ~ "del Lobero",
               loc_lower %in% tolower(c("Plastico")) ~ "El Plastico",
               loc_lower %in% tolower(c("Monument")) ~ "Gaviota",
               loc_lower %in% tolower(c("Punta Haydee")) ~ "Punta Haydee",
               loc_lower %in% tolower(c("taco", "Cerro Taco", "Hill 2")) ~ "Huillin",
               loc_lower %in% tolower(c("Broken Arrow")) ~ "Cerro Parmenio",
               loc_lower %in% tolower(c("Abismo", "ALC abismo")) ~ "Cerro El Abismo",
               loc_lower %in% tolower(c("Acantilado Damero")) ~ "Damero",
               loc_lower %in% tolower(c("E Diaguita")) ~ "Diaguita",
               loc_lower %in% tolower(c("lago oculto")) ~ "Lago Oculto",
               loc_lower %in% tolower(c("La Caverna", "Lacaverna", "La Cavarna", "Caverna")) ~ "La Caverna",

               # loc_lower %in% tolower(c(paste0("Lober", "\\u00ed", "a"), "Lobe ria", "loberia", "loberia central", "Loberia C", "Pl Lobería", "Lob C")) ~ "Loberia",
               loc_lower %in% tolower(c(paste0("Lober", "\\u00ed", "a"), "Lobe ria", "loberia", "loberia central", "Loberia C", paste0("Pl Lober", "\\u00ed", "a"), "Lob C")) ~ "Loberia",
               loc_lower %in% tolower(c("Loberia N", "Loberia-N", "Lobo N")) ~ "Loberia North",
               loc_lower %in% tolower(c("Loberia S", "Loberia S", "Loveria S", "Lob S")) ~ "Loberia South",
               loc_lower %in% tolower(c("Lob flats", "Loberia F", "Loberia flats", "Loberia flats S", "lobe ria valley", "Loberia Fl")) ~ "Loberia Flats",
               loc_lower %in% tolower(c("Lobe ria H", "Lob H")) ~ "Loberia Hill",

               loc_lower %in% tolower(c("Mad", "Mad S", "Mad N", "Mad high", "Mad (M)", "Mad-A", "Mad slope", "Md Der?")) ~ "Maderas",
               loc_lower %in% tolower(c("Mad (E)", "Mad(E)", "Mad-E", "Mad E high", "Mad E", "Maderas E")) ~ "Maderas East",
               loc_lower %in% tolower(c("Mad (C)", "Mad(C)", "Mad-C", "Mad C", "Mad C/W")) ~ "Maderas Central",
               loc_lower %in% tolower(c("Mad (W)", "Mad-w", "Mad w", "Mad(W)")) ~ "Maderas West",
               loc_lower %in% tolower(c("Mad Flats", "Mad F", "Mad flat", "Mad-F", "mad-flats", "above Maderas", "Maderas flat", "Maderas High")) ~ "Maderas Flats",
               loc_lower %in% tolower(c("Mad-r", "Mad R", "Mad Ridge", "Maderas Rdg", "HD R?", "Mad B")) ~ "Maderas Ridge",
               loc_lower %in% tolower(c("Mad Saddle")) ~ "Maderas Saddle",

               # loc_lower %in% tolower(c("Módulo", "PMod", "Modelo", "landing beach", "Mod", "modulo", "Pl El Modulo", "El Módulo", "Playa Modulo", "Above int'l highway", "International highway", "PMod", "Highway",
               loc_lower %in% tolower(c(paste0("M", "\\u00f3", "dulo"), "PMod", "Modelo", "landing beach", "Mod", "modulo", "Pl El Modulo", paste0("El M", "\\u00f3", "dulo"),
                                        "Playa Modulo", "Above int'l highway", "International highway", "PMod", "Highway",
                                        "Mod C", "Mod C flat", "Mod C high", "Mod E", "Mod E high", "Mod flat", "Mod H", "Mod low", "Mod W", "Mod W flats", "Mod W high",
                                        "Modulo", "Modulo Ridge", "P el Modulo", "Mod N", "Modulo C", "El Modelo")) ~ "Modulo",
               loc_lower %in% tolower(c("P Modulo N")) ~ "Modulo North",
               loc_lower %in% tolower(c("P Modulo S")) ~ "Modulo South",
               loc_lower %in% tolower(c("Lago Daniel", "Mod S", "Modulo S", "Mod South", "Mods")) ~ "Modulo South",
               loc_lower %in% tolower(c("Flats Modulo", "Modulo heights", "Mod Flats", "modulo flats", "Modulo Road", "El Condor/Modulo", "Modulo Flat",
                                        "modulo-camp", "Modulo/Camp", "Mod Fl", "Mod F")) ~ "Modulo Flats",

               loc_lower %in% tolower(c("Cabo Lodge", "Campamento", "Flats Daniel", "Daniel/Flats", "Daniel/Flats", "Chilean Camp", "Flat near camp", "camp", "Flats, Near Camp")) ~ "Camp",
               loc_lower %in% tolower(c("Playa Daniel", "daniel", "Danile", "Danilel", "Daniel H", "Plan Daniel", "P Daniel", "P Daniel high", "PDaniel", "P Daniel", "Planicie Daniel", "Dorsal Daniel", "Pl Daniel", "Daiel", "Daniel F")) ~ "Daniel",
               loc_lower %in% tolower(c("Playa Marko", "Marco", "Marko hill", "Marko H", "Plan Marko", "marko ridge", "PMarko", "P Marka", "P Marko", "marko", "Marko C", "Marko N", "Playa Marka", "marko/condor slope", "Maduo?")) ~ "Marko",
               loc_lower %in% tolower(c("El Condor Flats", "marko flats", "Flats Larga", "Larga flats", "Flats Marko")) ~ "Marko Flats",
               loc_lower %in% tolower(c("S Playa Largo", "Playa Larga High", "playa larga", "P Larga", "PLarga")) ~ "Larga",
               loc_lower %in% tolower(c("P Media Luna", "media luna", "Media Luna flats", "Medi Luna", "Meda Luna", "Meda Luna S", "Media L", "S Pl Media Luna", "M Luna", "MLuna", "MML", "ML", "towards Media Luna", "Mu Lu?")) ~ "Media Luna",

               loc_lower %in% tolower(c("Ivaldo", "Pl N Baldo", "Nivaldo", "Nibaldo Bahamondes", "P Nibaldo")) ~ "Nibaldo",
               loc_lower %in% tolower(c("PNegra", "PNegra east", "P Negra")) ~ "Punta Negra",
               loc_lower %in% tolower(c("P Elevado", "Paso elevado")) ~ "Paso Elevado",
               # loc_lower %in% tolower(c("Papúa", "Playa Papua", "P Papua")) ~ "Papua",
               loc_lower %in% tolower(c(paste0("Pap", "\\u00fa", "a"), "Playa Papua", "P Papua")) ~ "Papua",
               loc_lower %in% tolower(c("Pl Paulina", "West Coast central", "WST")) ~ "Paulina",
               loc_lower %in% tolower(c("Peng Col 12", "Col 14", "Colony 23", "Colony 20", "Colony 10", "Col 23")) ~ "Penguin Colonies",
               loc_lower %in% tolower(c("Pinochet", "P Pinochet de la Barra")) ~ "Pinochet de la Barra",
               loc_lower %in% tolower(c("Punta Pobleta", "Punta Poblete", "Poblete")) ~ "Punta Poblete",
               loc_lower %in% tolower(c("Pocitas", "P Pocitas")) ~ "Pocitas",
               loc_lower %in% tolower(c("Sciappacasse")) ~ "Schiappacasse",
               loc_lower %in% tolower(c("Punta Ventana")) ~ "Ventana",
               loc_lower %in% tolower(c("Flats Toqui", "Tokey")) ~ "Toqui",
               # loc_lower %in% tolower(c("Roquerío", "P Roquerio")) ~ "Roquerio",
               loc_lower %in% tolower(c(paste0("Roquer", "\\u00ed", "o"), "P Roquerio")) ~ "Roquerio",
               loc_lower %in% tolower(c("Remanso")) ~ "El Remanso",
               loc_lower %in% tolower(c("San Telmo", "San Telmo Norte", "P San Telmo", "Punta San Telmo")) ~ "PuntaSan Telmo",
               loc_lower %in% tolower(c("Valley Corto")) ~ "Valle Corto",
               # loc_lower %in% tolower(c("P Yamana", "Yámana", "Yamana", "Yamana Flats", "Pl Yamana", "Playa Yamana")) ~ "Yamana",
               loc_lower %in% tolower(c("P Yamana", paste0("Y", "\\u00e1", "mana"), "Yamana", "Yamana Flats", "Pl Yamana", "Playa Yamana")) ~ "Yamana",
               loc_lower %in% tolower(c("P Yuseff", "ML/P Yusseff", "P Yusseff", "Punta Yuseff", "Punta J")) ~ "Punta Yuseff",

               # "P Golondrina to P del Lobero", "P Delphin to P La Caverna", "P Paulina to P Aranda"
               # loc_lower %in% tolower(c("WST")) ~ "Unknown",

               .default = location
             )) %>%
    select(-c(loc_lower, loc_clean))
}
