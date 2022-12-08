
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
                                     "10YCY-1001A0003J", "10Y1001A1001A990"),
                        AreaTypeCode = c("CTA",
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
                                         "CTA", "CTY", "BZN", "CTA", "CTY", "BZN"),
                        AreaName = c("NOS BiH CA",
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
                                     "Cyprus TSO CA", "Cyprus", "MD BZ"),
                        MapCode = c("BA", "BA",
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
                                    "RS", "LV", "LV", "LV", "LU", "MD", "CY", "CY", "CY", "MD")),
                   class = "data.frame", row.names = c(NA, -124L))
  eic
}

#' Get the available day-ahead price areas.
#'
#' @export
#'
#'
en_transmission_day_ahead_prices_eic <- function(){
  trans_df <- structure(list(AreaCode = c("10YCS-SERBIATSOV", "10YHU-MAVIR----U",
                                          "10YSK-SEPS-----K", "10YRO-TEL------P", "10YES-REE------0", "10Y1001A1001A46L",
                                          "10YDK-1--------W", "10YNO-1--------2", "10YFI-1--------U", "10Y1001A1001A45N",
                                          "10Y1001A1001A44P", "10YNO-3--------J", "10YLV-1001A00074", "10YDK-2--------M",
                                          "10YNO-2--------T", "10Y1001A1001A47J", "10Y1001A1001A48H", "10YNO-4--------9",
                                          "10Y1001A1001A39I", "10YLT-1001A0008Q", "10YPT-REN------W", "10YSI-ELES-----O",
                                          "10Y1001A1001A82H", "10YFR-RTE------C", "10YAT-APG------L", "10YBE----------2",
                                          "10YNL----------L", "10YGB----------A", "10YCA-BULGARIA-R", "10YHR-HEP------M",
                                          "10YPL-AREA-----S", "10YGR-HTSO-----Y", "10Y1001A1001A70O", "10Y1001A1001A73I",
                                          "10Y1001A1001A71M", "10Y1001A1001A788", "10Y1001A1001A77A", "10Y1001A1001A75E",
                                          "10Y1001A1001A893", "10Y1001A1001A74G", "10Y1001A1001A885", "10YCH-SWISSGRIDZ",
                                          "10YCZ-CEPS-----N", "10Y1001A1001A59C"),
                             AreaTypeCode = c("BZN",
                                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN"),
                             AreaName = c("EMS BZ",
                                          "MAVIR BZ", "SEPS BZ", "Transelectrica BZ", "REE BZ", "SE3 BZ",
                                          "DK1  BZ", "NO1 BZ", "Fingrid BZ", "SE2 BZ", "SE1 BZ", "NO3 BZ",
                                          "AST BZ", "DK2 BZ", "NO2 BZ", "SE4 BZ", "NO5 BZ", "NO4 BZ", "Elering BZ",
                                          "Litgrid BZ", "REN BZ", "ELES BZ", "DE-LU BZ", "RTE BZ", "APG BZ",
                                          "Elia BZ", "TenneT NL BZ", "National Grid BZ", "ESO BZ", "HOPS BZ",
                                          "PSE SA BZ", "IPTO BZ", "IT-Centre-North BZ", "IT-North BZ",
                                          "IT-Centre-South BZ", "IT-South BZ", "IT-Rossano BZ", "IT-Sicily BZ",
                                          "Italy_Sacodc", "IT-Sardinia BZ", "Italy_Saco_AC", "swissgrid BZ",
                                          "CEPS BZ", "Ireland - (SEM) BZ")),
                        class = c("tbl_df", "tbl","data.frame"),
                        row.names = c(NA, -44L))

  trans_df
}

#' Get the day-ahead commercial schedules EIC codes
#'
#' @export
#'
#'
en_transmission_day_ahead_commercial_schedules_eic <- function(){
  structure(
    list( OutAreaCode = c("10YLT-1001A0008Q", "10YNO-4--------9",
                          "10Y1001A1001A39I", "10Y1001A1001A45N", "10YNO-4--------9", "10YPL-AREA-----S",
                          "10YFI-1--------U", "10YDK-2--------M", "10Y1001A1001A47J", "10YAT-APG------L",
                          "10YSI-ELES-----O", "10Y1001A1001A47J", "10YIT-GRTN-----B", "10Y1001A1001A39I",
                          "10YLV-1001A00074", "10YNO-3--------J", "10YNO-2--------T", "10Y1001A1001A73I",
                          "10YNL----------L", "10YNO-4--------9", "10Y1001A1001A45N", "10Y1001A1001A71M",
                          "10Y1001A1001A70O", "10Y1001A1001A47J", "10YSI-ELES-----O", "10YNL----------L",
                          "10YFI-1--------U", "10YDE-VE-------2", "10YFI-1--------U", "10Y1001A1001A44P",
                          "10Y1001A1001A44P", "10Y1001A1001A016", "10YIE-1001A00010", "10YGB----------A",
                          "10YFR-RTE------C", "10YLV-1001A00074", "10YFR-RTE------C", "10Y1001A1001A48H",
                          "10YGB----------A", "10Y1001A1001A82H", "10Y1001A1001A70O", "10Y1001A1001A71M",
                          "10Y1001A1001A893", "10Y1001A1001A75E", "10Y1001A1001A788", "10Y1001A1001A70O",
                          "10Y1001A1001A71M", "10Y1001A1001A74G", "10YCZ-CEPS-----N", "10YHU-MAVIR----U",
                          "10YLT-1001A0008Q", "10Y1001A1001A796", "10YFR-RTE------C", "10YIT-GRTN-----B",
                          "10Y1001A1001A50U", "10Y1001A1001A893", "10YNO-2--------T", "10Y1001A1001A51S",
                          "10Y1001A1001A77A", "10YLT-1001A0008Q", "10YDK-1--------W", "10Y1001A1001A44P",
                          "10Y1001A1001A46L", "10Y1001A1001A47J", "10YAT-APG------L", "10YPL-AREA-----S",
                          "10YPL-AREA-----S", "10YDK-1--------W", "10Y1001A1001A48H", "10YSI-ELES-----O",
                          "10Y1001A1001A45N", "10YDK-1--------W", "10YNO-1--------2", "10Y1001A1001A46L",
                          "10YES-REE------0", "10Y1001A1001A46L", "10YGB----------A", "10YNO-1--------2",
                          "10Y1001A1001A796", "10YNO-2--------T", "10YLT-1001A0008Q", "10YPT-REN------W",
                          "10Y1001A1001A46L", "10YGB----------A", "10Y1001A1001A47J", "10YNL----------L",
                          "10Y1001A1001A48H", "10YDOM-PL-SE-LT2", "10YGB----------A", "10YBE----------2",
                          "10YDE-EON------1", "10YES-REE------0", "10YHR-HEP------M", "10Y1001A1001A788",
                          "10YIT-GRTN-----B", "10Y1001A1001A77A", "10Y1001A1001A74G", "10YDK-2--------M",
                          "10YLT-1001A0008Q", "10YHU-MAVIR----U", "10YSK-SEPS-----K", "10Y1001A1001A46L",
                          "10YDK-1--------W", "10YNO-1--------2", "10YSK-SEPS-----K", "10YNO-2--------T",
                          "10YNO-3--------J", "10YRO-TEL------P", "10YNO-4--------9", "10YNO-3--------J",
                          "10YFI-1--------U", "10YNO-1--------2", "10YNO-3--------J", "10Y1001A1001A45N" ),
          OutAreaTypeCode = c("BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "BZN", "BZN", "BZN", "CTA", "CTA", "BZN", "CTA", "BZN", "BZN",
                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "CTA", "BZN", "BZN", "CTA", "BZN", "BZN", "BZN", "CTA", "CTA",
                              "CTA", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "CTA", "CTA", "CTA", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "BZN", "BZN", "BZN", "BZN", "CTA", "BZA", "BZN", "BZN", "BZN",
                              "CTA", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "BZN",
                              "CTA", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "BZA", "BZN", "BZN", "CTA", "BZN", "CTA", "BZN", "CTA", "BZN",
                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                              "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN" ),
          OutAreaName = c("Litgrid BZ", "NO4 BZ", "Elering BZ", "SE2 BZ",
                          "NO4 BZ", "PSE SA BZ", "Fingrid BZ", "DK2 BZ", "SE4 BZ", "APG CA",
                          "ELES CA", "SE4 BZ", "Italy CA", "Elering BZ", "AST BZ", "NO3 BZ",
                          "NO2 BZ", "IT-North BZ", "TenneT NL BZ", "NO4 BZ", "SE2 BZ",
                          "IT-Centre-South BZ", "IT-Centre-North BZ", "SE4 BZ", "ELES CA",
                          "TenneT NL BZ", "Fingrid BZ", "50Hertz CA", "Fingrid BZ", "SE1 BZ",
                          "SE1 BZ", "SONI CA", "EirGrid CA", "National Grid CA", "RTE BZ",
                          "AST BZ", "RTE BZ", "NO5 BZ", "National Grid BZ", "DE-LU BZ",
                          "IT-Centre-North BZ", "IT-Centre-South BZ", "Italy_Sacodc", "IT-Sicily BZ",
                          "IT-South BZ", "IT-Centre-North BZ", "IT-Centre-South BZ", "IT-Sardinia BZ",
                          "CEPS BZ", "MAVIR BZ", "Litgrid BZ", "Energinet CA", "RTE CA",
                          "Italy CA", "Kaliningrad BZ", "Italy_Sacodc", "NO2 BZ", "Belarus BZ",
                          "IT-Rossano BZ", "Litgrid BZ", "DK1  BZ", "SE1 BZ", "SE3 BZ",
                          "SE4 BZ", "APG CA", "PSE SA BZA", "PSE SA BZ", "DK1  BZ", "NO5 BZ",
                          "ELES CA", "SE2 BZ", "DK1  BZ", "NO1 BZ", "SE3 BZ", "REE BZ",
                          "SE3 BZ", "National Grid CA", "NO1 BZ", "Energinet CA", "NO2 BZ",
                          "Litgrid BZ", "REN BZ", "SE3 BZ", "National Grid BZ", "SE4 BZ",
                          "TenneT NL BZ", "NO5 BZ", "BZA LT-SE4", "National Grid BZ", "Elia BZ",
                          "TenneT GER CA", "REE BZ", "HOPS CA", "IT-South BZ", "Italy CA",
                          "IT-Rossano BZ", "IT-Sardinia BZ", "DK2 BZ", "Litgrid BZ", "MAVIR BZ",
                          "SEPS BZ", "SE3 BZ", "DK1  BZ", "NO1 BZ", "SEPS BZ", "NO2 BZ",
                          "NO3 BZ", "Transelectrica BZ", "NO4 BZ", "NO3 BZ", "Fingrid BZ",
                          "NO1 BZ", "NO3 BZ", "SE2 BZ"),
          OutMapCode = c("LT", "NO4", "EE",
                         "SE2", "NO4", "PL", "FI", "DK2", "SE4", "AT", "SI", "SE4", "IT",
                         "EE", "LV", "NO3", "NO2", "IT_North", "NL", "NO4", "SE2", "IT_CSUD",
                         "IT_CNOR", "SE4", "SI", "NL", "FI", "DE_50HzT", "FI", "SE1",
                         "SE1", "NIE", "IE", "GB", "FR", "LV", "FR", "NO5", "GB", "DE_LU",
                         "IT_CNOR", "IT_CSUD", "IT_SACO_DC", "IT_SICI", "IT_SUD", "IT_CNOR",
                         "IT_CSUD", "IT_SARD", "CZ", "HU", "LT", "DK", "FR", "IT", "RU_KGD",
                         "IT_SACO_DC", "NO2", "BY", "IT_ROSN", "LT", "DK1", "SE1", "SE3",
                         "SE4", "AT", "PL", "PL", "DK1", "NO5", "SI", "SE2", "DK1", "NO1",
                         "SE3", "ES", "SE3", "GB", "NO1", "DK", "NO2", "LT", "PT", "SE3",
                         "GB", "SE4", "NL", "NO5", "LT-SE4", "GB", "BE", "DE_TenneT_GER",
                         "ES", "HR", "IT_SUD", "IT", "IT_ROSN", "IT_SARD", "DK2", "LT",
                         "HU", "SK", "SE3", "DK1", "NO1", "SK", "NO2", "NO3", "RO", "NO4",
                         "NO3", "FI", "NO1", "NO3", "SE2"),
          InAreaCode = c("10Y1001A1001A47J",
                         "10Y1001A1001A44P", "10YFI-1--------U", "10Y1001A1001A44P", "10Y1001A1001A45N",
                         "10Y1001A1001A47J", "10Y1001A1001A39I", "10Y1001A1001A47J", "10YPL-AREA-----S",
                         "10YSI-ELES-----O", "10YAT-APG------L", "10Y1001A1001A82H", "10YAT-APG------L",
                         "10YLV-1001A00074", "10Y1001A1001A39I", "10Y1001A1001A48H", "10Y1001A1001A48H",
                         "10Y1001A1001A70O", "10YNO-2--------T", "10YFI-1--------U", "10Y1001A1001A46L",
                         "10Y1001A1001A74G", "10Y1001A1001A893", "10Y1001A1001A46L", "10YHR-HEP------M",
                         "10YDK-1--------W", "10Y1001A1001A44P", "10Y1001A1001A796", "10Y1001A1001A46L",
                         "10YNO-4--------9", "10Y1001A1001A45N", "10YGB----------A", "10YGB----------A",
                         "10YIE-1001A00010", "10YES-REE------0", "10YLT-1001A0008Q", "10YGB----------A",
                         "10YNO-1--------2", "10YNL----------L", "10Y1001A1001A47J", "10Y1001A1001A73I",
                         "10Y1001A1001A70O", "10Y1001A1001A70O", "10Y1001A1001A77A", "10Y1001A1001A77A",
                         "10Y1001A1001A71M", "10Y1001A1001A788", "10Y1001A1001A71M", "10YSK-SEPS-----K",
                         "10YRO-TEL------P", "10Y1001A1001A51S", "10YDE-EON------1", "10YIT-GRTN-----B",
                         "10YSI-ELES-----O", "10YLT-1001A0008Q", "10Y1001A1001A74G", "10YNO-1--------2",
                         "10YLT-1001A0008Q", "10Y1001A1001A788", "10YPL-AREA-----S", "10Y1001A1001A46L",
                         "10YFI-1--------U", "10YFI-1--------U", "10YDK-2--------M", "10YIT-GRTN-----B",
                         "10YDOM-PL-SE-LT2", "10YLT-1001A0008Q", "10YNL----------L", "10YNO-3--------J",
                         "10YIT-GRTN-----B", "10YNO-4--------9", "10YNO-2--------T", "10Y1001A1001A48H",
                         "10Y1001A1001A45N", "10YPT-REN------W", "10YNO-1--------2", "10Y1001A1001A016",
                         "10Y1001A1001A46L", "10YDE-VE-------2", "10YDK-1--------W", "10YLV-1001A00074",
                         "10YES-REE------0", "10YDK-1--------W", "10YFR-RTE------C", "10YLT-1001A0008Q",
                         "10YGB----------A", "10YNO-2--------T", "10YPL-AREA-----S", "10YBE----------2",
                         "10YGB----------A", "10Y1001A1001A796", "10YFR-RTE------C", "10YSI-ELES-----O",
                         "10Y1001A1001A71M", "10YFR-RTE------C", "10Y1001A1001A75E", "10Y1001A1001A893",
                         "10YDK-1--------W", "10Y1001A1001A50U", "10YSK-SEPS-----K", "10YCZ-CEPS-----N",
                         "10Y1001A1001A47J", "10YDK-2--------M", "10YNO-2--------T", "10YHU-MAVIR----U",
                         "10YNL----------L", "10YNO-4--------9", "10YHU-MAVIR----U", "10YNO-3--------J",
                         "10YNO-1--------2", "10YNO-4--------9", "10YNO-3--------J", "10Y1001A1001A45N",
                         "10YNO-3--------J"),
          InAreaTypeCode = c("BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "CTA", "BZN",
                             "CTA", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "CTA", "BZN", "BZN", "CTA", "BZN", "BZN",
                             "BZN", "CTA", "CTA", "CTA", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "CTA", "CTA", "CTA", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "CTA", "BZA",
                             "BZN", "BZN", "BZN", "CTA", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "CTA", "BZN", "CTA", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "BZA", "BZN", "BZN", "CTA", "BZN", "CTA",
                             "BZN", "CTA", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN", "BZN",
                             "BZN", "BZN", "BZN"),
          InAreaName = c("SE4 BZ", "SE1 BZ", "Fingrid BZ",
                         "SE1 BZ", "SE2 BZ", "SE4 BZ", "Elering BZ", "SE4 BZ", "PSE SA BZ",
                         "ELES CA", "APG CA", "DE-LU BZ", "APG CA", "AST BZ", "Elering BZ",
                         "NO5 BZ", "NO5 BZ", "IT-Centre-North BZ", "NO2 BZ", "Fingrid BZ",
                         "SE3 BZ", "IT-Sardinia BZ", "Italy_Sacodc", "SE3 BZ", "HOPS CA",
                         "DK1  BZ", "SE1 BZ", "Energinet CA", "SE3 BZ", "NO4 BZ", "SE2 BZ",
                         "National Grid CA", "National Grid CA", "EirGrid CA", "REE BZ",
                         "Litgrid BZ", "National Grid BZ", "NO1 BZ", "TenneT NL BZ", "SE4 BZ",
                         "IT-North BZ", "IT-Centre-North BZ", "IT-Centre-North BZ", "IT-Rossano BZ",
                         "IT-Rossano BZ", "IT-Centre-South BZ", "IT-South BZ", "IT-Centre-South BZ",
                         "SEPS BZ", "Transelectrica BZ", "Belarus BZ", "TenneT GER CA",
                         "Italy CA", "ELES CA", "Litgrid BZ", "IT-Sardinia BZ", "NO1 BZ",
                         "Litgrid BZ", "IT-South BZ", "PSE SA BZ", "SE3 BZ", "Fingrid BZ",
                         "Fingrid BZ", "DK2 BZ", "Italy CA", "BZA LT-SE4", "Litgrid BZ",
                         "TenneT NL BZ", "NO3 BZ", "Italy CA", "NO4 BZ", "NO2 BZ", "NO5 BZ",
                         "SE2 BZ", "REN BZ", "NO1 BZ", "SONI CA", "SE3 BZ", "50Hertz CA",
                         "DK1  BZ", "AST BZ", "REE BZ", "DK1  BZ", "RTE BZ", "Litgrid BZ",
                         "National Grid BZ", "NO2 BZ", "PSE SA BZA", "Elia BZ", "National Grid BZ",
                         "Energinet CA", "RTE BZ", "ELES CA", "IT-Centre-South BZ", "RTE CA",
                         "IT-Sicily BZ", "Italy_Sacodc", "DK1  BZ", "Kaliningrad BZ",
                         "SEPS BZ", "CEPS BZ", "SE4 BZ", "DK2 BZ", "NO2 BZ", "MAVIR BZ",
                         "TenneT NL BZ", "NO4 BZ", "MAVIR BZ", "NO3 BZ", "NO1 BZ", "NO4 BZ",
                         "NO3 BZ", "SE2 BZ", "NO3 BZ"),
          InMapCode = c("SE4", "SE1", "FI",
                        "SE1", "SE2", "SE4", "EE", "SE4", "PL", "SI", "AT", "DE_LU",
                        "AT", "LV", "EE", "NO5", "NO5", "IT_CNOR", "NO2", "FI", "SE3",
                        "IT_SARD", "IT_SACO_DC", "SE3", "HR", "DK1", "SE1", "DK", "SE3",
                        "NO4", "SE2", "GB", "GB", "IE", "ES", "LT", "GB", "NO1", "NL",
                        "SE4", "IT_North", "IT_CNOR", "IT_CNOR", "IT_ROSN", "IT_ROSN",
                        "IT_CSUD", "IT_SUD", "IT_CSUD", "SK", "RO", "BY", "DE_TenneT_GER",
                        "IT", "SI", "LT", "IT_SARD", "NO1", "LT", "IT_SUD", "PL", "SE3",
                        "FI", "FI", "DK2", "IT", "LT-SE4", "LT", "NL", "NO3", "IT", "NO4",
                        "NO2", "NO5", "SE2", "PT", "NO1", "NIE", "SE3", "DE_50HzT", "DK1",
                        "LV", "ES", "DK1", "FR", "LT", "GB", "NO2", "PL", "BE", "GB",
                        "DK", "FR", "SI", "IT_CSUD", "FR", "IT_SICI", "IT_SACO_DC", "DK1",
                        "RU_KGD", "SK", "CZ", "SE4", "DK2", "NO2", "HU", "NL", "NO4",
                        "HU", "NO3", "NO1", "NO4", "NO3", "SE2", "NO3")),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -114L))
}

#' Get the generation codes
#'
#' @export
#'
en_generation_codes <- function(){

  structure(list(codes = c("A03", "A04", "A05", "B01", "B02", "B03",
                           "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12",
                           "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", "B21",
                           "B22", "B23", "B24"),
                 meaning = c("Mixed", "Generation", "Load",
                             "Biomass", "Fossil Brown coal/Lignite", "Fossil Coal-derived gas",
                             "Fossil Gas", "Fossil Hard coal", "Fossil Oil", "Fossil Oil shale",
                             "Fossil Peat", "Geothermal", "Hydro Pumped Storage", "Hydro Run-of-river and poundage",
                             "Hydro Water Reservoir", "Marine", "Nuclear", "Other renewable",
                             "Solar", "Waste", "Wind Offshore", "Wind Onshore", "Other", "AC Link",
                             "DC Link", "Substation", "Transformer"),
                 co2_g_kwh = c(NA, NA,
                               NA, 390, 360, NA, 200, 340, 260, NA, 380, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                 efficiency = c(NA,
                                NA, NA, NA, 0.35, NA, 0.5, 0.38, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
            row.names = c(NA, -27L),
            class = "data.frame")
}

#' Get Party_X EIC codes
#'
#' This function downloads approved Party_X Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_party <- party_eic()
#' @export
#'
party_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/X_eiccodes.csv")


#' Get Area_Y EIC codes
#'
#' This function downloads approved Area_Y Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_area <- area_eic()
#' @export
#'
area_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Y_eiccodes.csv")


#' Get Accounting_point_Z EIC codes
#'
#' This function downloads approved Accounting_point_Z Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_accounting_point <- accounting_point_eic()
#' @export
#'
accounting_point_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Z_eiccodes.csv")


#' Get Tie_Line_T EIC codes
#'
#' This function downloads approved Tie_Line_T Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_tie_line <- tie_line_eic()
#' @export
#'
tie_line_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/T_eiccodes.csv")


#' Get Location_V EIC codes
#'
#' This function downloads approved Location_V Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_location <- location_eic()
#' @export
#'
location_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/V_eiccodes.csv")


#' Get Resource_Object_W EIC codes
#'
#' This function downloads approved Resource_Object_W Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_resource_object <- resource_object_eic()
#' @export
#'
resource_object_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/W_eiccodes.csv")


#' Get Substation_A EIC codes
#'
#' This function downloads all approved Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_substation <- substation_eic()
#' @export
#'
substation_eic <- function() get_eiccodes(f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/A_eiccodes.csv")


#' Get all EIC codes
#'
#' This function downloads approved all Energy Identification Codes from
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes/
#'
#' @examples
#' eic_all <- all_eic()
#' @export
#'
all_eic <- function() {
  tibble::as_tibble(data.table::rbindlist(l = list(party_eic(),
                                                   area_eic(),
                                                   accounting_point_eic(),
                                                   tie_line_eic(),
                                                   location_eic(),
                                                   resource_object_eic(),
                                                   substation_eic()),
                                            use.names = TRUE, fill = TRUE))
}
