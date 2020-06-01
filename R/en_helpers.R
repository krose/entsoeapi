
#' Get eic codes
#'
#' @export
#'
#'
en_eic <- function(){

  eic <- structure(list(AreaCode = c("10YBA-JPCC-----D", "10YBA-JPCC-----D",
                              "10YBA-JPCC-----D", "10YCS-CG-TSO---S", "10YCS-CG-TSO---S", "10YCS-CG-TSO---S",
                              "10YCH-SWISSGRIDZ", "10YCH-SWISSGRIDZ", "10YCH-SWISSGRIDZ", "10YNL----------L",
                              "10YHU-MAVIR----U", "10YAT-APG------L", "10YAT-APG------L", "10YHU-MAVIR----U",
                              "10YHU-MAVIR----U", "10YPL-AREA-----S", "10YPL-AREA-----S", "10YNO-4--------9",
                              "10YNO-3--------J", "10YAT-APG------L", "10Y1001A1001A82H", "10Y1001A1001A48H",
                              "10YNO-1--------2", "10YLU-CEGEDEL-NQ", "10YNO-0--------C", "10YNO-2--------T",
                              "10YNO-0--------C", "10YDE-RWENET---I", "10YPT-REN------W", "10YDE-EON------1",
                              "10YPT-REN------W", "10YPT-REN------W", "10Y1001A1001A83F", "10Y1001A1001A39I",
                              "10Y1001A1001A39I", "10Y1001A1001A39I", "10YSK-SEPS-----K", "10Y1001A1001A47J",
                              "10Y1001A1001A45N", "10Y1001A1001A46L", "10Y1001A1001A44P", "10YGB----------A",
                              "10YSE-1--------K", "GB", "10YSK-SEPS-----K", "10YSK-SEPS-----K",
                              "10YBE----------2", "10YGB----------A", "10YSE-1--------K", "10YDK-2--------M",
                              "10YDK-1--------W", "10YDE-VE-------2", "10YBE----------2", "10YBE----------2",
                              "10YLT-1001A0008Q", "10Y1001A1001A65H", "10Y1001A1001A796", "10YLT-1001A0008Q",
                              "10YLT-1001A0008Q", "10Y1001A1001B012", "10YCA-BULGARIA-R", "10YCA-BULGARIA-R",
                              "10YCA-BULGARIA-R", "10Y1001A1001B012", "10Y1001A1001B012", "10YGR-HTSO-----Y",
                              "10YGR-HTSO-----Y", "10YSI-ELES-----O", "10YSI-ELES-----O", "10YSI-ELES-----O",
                              "10YPL-AREA-----S", "10YGR-HTSO-----Y", "10YCZ-CEPS-----N", "10YCZ-CEPS-----N",
                              "10YCZ-CEPS-----N", "10YNL----------L", "10YHR-HEP------M", "10YHR-HEP------M",
                              "10YNL----------L", "10YHR-HEP------M", "10YMK-MEPSO----8", "10YMK-MEPSO----8",
                              "10YMK-MEPSO----8", "10YFI-1--------U", "10YFI-1--------U", "10YFI-1--------U",
                              "10YUA-WEPS-----0", "10Y1001C--000182", "10Y1001C--00003F", "10Y1001C--00003F",
                              "10YES-REE------0", "10YES-REE------0", "10YES-REE------0", "10Y1001A1001A016",
                              "10YDE-ENBW-----N", "10YIE-1001A00010", "10Y1001A1001A59C", "10YFR-RTE------C",
                              "10YFR-RTE------C", "10YFR-RTE------C", "10Y1001A1001A70O", "10YIE-1001A00010",
                              "10Y1001A1001A75E", "10Y1001A1001A788", "10Y1001A1001A71M", "10Y1001A1001A73I",
                              "10Y1001A1001A74G", "10YIT-GRTN-----B", "10YIT-GRTN-----B", "10YRO-TEL------P",
                              "10YRO-TEL------P", "10YRO-TEL------P", "10YCS-SERBIATSOV", "10YCS-SERBIATSOV",
                              "10YCS-SERBIATSOV", "10YLV-1001A00074", "10YLV-1001A00074", "10YLV-1001A00074",
                              "10YLU-CEGEDEL-NQ", "10Y1001A1001A990", "10YCY-1001A0003J", "10YCY-1001A0003J",
                              "10YCY-1001A0003J", "10Y1001A1001A990"), AreaTypeCode = c("CTA",
                                                                                        "CTY", "BZN", "CTA", "CTY", "BZN", "CTA", "BZN", "CTY", "BZN",
                                                                                        "CTA", "CTA", "CTY", "CTY", "BZN", "CTY", "BZN", "BZN", "BZN",
                                                                                        "BZN", "BZN", "BZN", "BZN", "CTY", "CTA", "BZN", "CTY", "CTA",
                                                                                        "CTA", "CTA", "CTY", "BZN", "CTY", "CTA", "CTY", "BZN", "CTA",
                                                                                        "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "CTY", "BZN", "CTY",
                                                                                        "CTA", "CTA", "CTY", "BZN", "BZN", "CTA", "CTY", "BZN", "CTA",
                                                                                        "CTY", "CTA", "BZN", "CTY", "CTA", "CTA", "BZN", "CTY", "BZN",
                                                                                        "CTY", "CTA", "BZN", "CTA", "BZN", "CTY", "CTA", "CTY", "CTA",
                                                                                        "BZN", "CTY", "CTA", "CTA", "BZN", "CTY", "CTY", "CTA", "CTY",
                                                                                        "BZN", "CTA", "BZN", "CTY", "CTA", "CTA", "BZN", "CTY", "CTA",
                                                                                        "BZN", "CTY", "CTA", "CTA", "CTA", "BZN", "CTA", "BZN", "CTY",
                                                                                        "BZN", "CTY", "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "CTY",
                                                                                        "CTA", "BZN", "CTY", "CTA", "CTY", "BZN", "CTA", "CTY", "BZN",
                                                                                        "CTA", "CTY", "BZN", "CTA", "CTY", "BZN"), AreaName = c("NOS BiH CA",
                                                                                                                                                "Bosnia Herzegovina", "NOS BiH BZ", "CGES CA", "Montenegro",
                                                                                                                                                "CGES BZ", "swissgrid CA", "swissgrid BZ", "Switzerland", "TenneT NL BZ",
                                                                                                                                                "MAVIR CA", "APG CA", "Austria", "Hungary", "MAVIR BZ", "Poland",
                                                                                                                                                "PSE SA BZ", "NO4 BZ", "NO3 BZ", "APG BZ", "DE-LU BZ", "NO5 BZ",
                                                                                                                                                "NO1 BZ", "Luxembourg", "Statnett CA", "NO2 BZ", "Norway", "Amprion CA",
                                                                                                                                                "REN CA", "TenneT GER CA", "Portugal", "REN BZ", "Germany", "Elering CA",
                                                                                                                                                "Estonia", "Elering BZ", "SEPS CA", "SE4 BZ", "SE2 BZ", "SE3 BZ",
                                                                                                                                                "SE1 BZ", "National Grid BZ", "SvK CA", "United Kingdom", "SEPS BZ",
                                                                                                                                                "Slovakia", "Elia CA", "National Grid CA", "Sweden", "DK2 BZ",
                                                                                                                                                "DK1  BZ", "50Hertz CA", "Belgium", "Elia BZ", "Litgrid CA",
                                                                                                                                                "Denmark", "Energinet CA", "Litgrid BZ", "Lithuania", "GSE CA",
                                                                                                                                                "ESO CA", "ESO BZ", "Bulgaria", "GSE BZN", "Georgia", "IPTO CA",
                                                                                                                                                "IPTO BZ", "ELES CA", "ELES BZ", "Slovenia", "PSE SA CA", "Greece",
                                                                                                                                                "CEPS CA", "CEPS BZ", "Czech Republic", "TenneT NL CA", "HOPS CA",
                                                                                                                                                "HOPS BZ", "Netherlands", "Croatia", "MEPSO CA", "North Macedonia",
                                                                                                                                                "MEPSO BZ", "Fingrid CA", "Fingrid BZ", "Finland", "Ukraine BEI CA",
                                                                                                                                                "Ukraine IPS CA", "Ukraine BZN", "Ukraine", "REE CA", "REE BZ",
                                                                                                                                                "Spain", "SONI CA", "TransnetBW CA", "EirGrid CA", "Ireland - (SEM) BZ",
                                                                                                                                                "RTE CA", "RTE BZ", "France", "IT-Centre-North BZ", "Ireland",
                                                                                                                                                "IT-Sicily BZ", "IT-South BZ", "IT-Centre-South BZ", "IT-North BZ",
                                                                                                                                                "IT-Sardinia BZ", "Italy CA", "Italy", "Transelectrica CA", "Transelectrica BZ",
                                                                                                                                                "Romania", "EMS CA", "Serbia", "EMS BZ", "AST CA", "Latvia",
                                                                                                                                                "AST BZ", "CREOS CA", "Republic of Moldova", "Cyprus TSO BZ",
                                                                                                                                                "Cyprus TSO CA", "Cyprus", "MD BZ"), MapCode = c("BA", "BA",
                                                                                                                                                                                                 "BA", "ME", "ME", "ME", "CH", "CH", "CH", "NL", "HU", "AT", "AT",
                                                                                                                                                                                                 "HU", "HU", "PL", "PL", "NO4", "NO3", "AT", "DE_LU", "NO5", "NO1",
                                                                                                                                                                                                 "LU", "NO", "NO2", "NO", "DE_Amprion", "PT", "DE_TenneT_GER",
                                                                                                                                                                                                 "PT", "PT", "DE", "EE", "EE", "EE", "SK", "SE4", "SE2", "SE3",
                                                                                                                                                                                                 "SE1", "GB", "SE", "GB", "SK", "SK", "BE", "GB", "SE", "DK2",
                                                                                                                                                                                                 "DK1", "DE_50HzT", "BE", "BE", "LT", "DK", "DK", "LT", "LT",
                                                                                                                                                                                                 "GE", "BG", "BG", "BG", "GE", "GE", "GR", "GR", "SI", "SI", "SI",
                                                                                                                                                                                                 "PL", "GR", "CZ", "CZ", "CZ", "NL", "HR", "HR", "NL", "HR", "MK",
                                                                                                                                                                                                 "MK", "MK", "FI", "FI", "FI", "UA_BEI", "UA_IPS", "UA", "UA",
                                                                                                                                                                                                 "ES", "ES", "ES", "NIE", "DE_TransnetBW", "IE", "IE_SEM", "FR",
                                                                                                                                                                                                 "FR", "FR", "IT_CNOR", "IE", "IT_SICI", "IT_SUD", "IT_CSUD",
                                                                                                                                                                                                 "IT_North", "IT_SARD", "IT", "IT", "RO", "RO", "RO", "RS", "RS",
                                                                                                                                                                                                 "RS", "LV", "LV", "LV", "LU", "MD", "CY", "CY", "CY", "MD")), class = "data.frame", row.names = c(NA,
                                                                                                                                                                                                                                                                                                   -124L))
  eic
}

#' Get the generation codes
#'
#' @export
#'
en_generation_codes <- function(){

  structure(list(codes = c("A03", "A04", "A05", "B01", "B02", "B03",
                           "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12",
                           "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", "B21",
                           "B22", "B23", "B24"), meaning = c("Mixed", "Generation", "Load",
                                                             "Biomass", "Fossil Brown coal/Lignite", "Fossil Coal-derived gas",
                                                             "Fossil Gas", "Fossil Hard coal", "Fossil Oil", "Fossil Oil shale",
                                                             "Fossil Peat", "Geothermal", "Hydro Pumped Storage", "Hydro Run-of-river and poundage",
                                                             "Hydro Water Reservoir", "Marine", "Nuclear", "Other renewable",
                                                             "Solar", "Waste", "Wind Offshore", "Wind Onshore", "Other", "AC Link",
                                                             "DC Link", "Substation", "Transformer")), class = "data.frame", row.names = c(NA,
                                                                                                                                           -27L))

}



