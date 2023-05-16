
# TODO: Documenting this function. But its works. # change some variables
# ! SOP_std = (WATERg + PROCNTg + FAT_g_standardised + CHOAVLg + FIBTGg + ALCg +ASHg)
SOP_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "WATERg", "PROCNTg", "FATg", # Change FAT_g_standardised to FATg
        "CHOAVLDFg", "FIBTGg", "ALCg", "ASHg" # change ASHg_std to ASHg
    )
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        # ! Create a temp row with the number of NAs across the required column
        mutate(temp = rowSums(is.na(dataset %>%
            select(all_of(columns))))) %>%
        rowwise() %>%
        # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
        mutate(SOP_std = ifelse(temp == length(columns), NA, sum(WATERg,
            PROCNTg,
            FAT_g_standardised,
            CHOAVLg,
            FIBTGg,
            ALCg,
            ASHg_std,
            na.rm = TRUE
        ))) %>%
        # ! remove the temp column
        select(-temp) %>%
        ungroup(), error = function(e) {
        print(paste0("Error : Required columns i.e. ", columns, " should be numeric. The SOP_std will not be calculated"))
    })
}

#  Carotene Eq.
# Weighted sum of the listed variables
# CARTBEQmcg_std <- 1 * CARTBmcg + 0.5 * CARTAmcg + 0.5 * CRYPXBmcg
# TODO: Create documentation. Function works. Most fileds
CARTBEQmcg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "CARTBmcg", "CARTAmcg", "CRYPXBmcg"
    )
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        # ! Create a temp row with the number of NAs across the required column
        mutate(temp = rowSums(is.na(dataset %>%
            select(all_of(columns))))) %>%
        rowwise() %>%
        # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
        mutate(CARTBEQmcg_std = ifelse(temp == length(columns), NA, sum(1 * CARTBmcg, 0.5 * CARTAmcg, 0.5 * CRYPXBmcg, na.rm = TRUE))) %>%
        # ! remove the temp column
        select(-temp) %>%
        ungroup(), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
        print("The SOP_std will not be calculated")
    })
}

#  Vitamin A, retinol activity eq.
# Weighted sum of the listed variables
# ! VITA_RAEmcg_std <- RETOLmcg + 1 / 12 * CARTBEQmcg
# TODO: Create documentation. Function works. Most fileds
VITA_RAEmcg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "RETOLmcg", "CARTBEQmcg"
    )
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        # ! Create a temp row with the number of NAs across the required column
        mutate(temp = rowSums(is.na(dataset %>%
            select(all_of(columns))))) %>%
        rowwise() %>%
        # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
        mutate(VITA_RAEmcg_std = ifelse(temp == length(columns), NA, sum(RETOLmcg, (1 / 12 * CARTBEQmcg), na.rm = TRUE))) %>% # ! remove the temp column
        select(-temp) %>%
        ungroup(), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
        print("The SOP_std will not be calculated")
    })
}

# Vitamin A, retinol eq.
# Weighted sum of the listed variables
# ! VITAmcg_std <- RETOLmcg + 1 / 6 * CARTBEQmcg
# TODO: Create documentation. Function works. Most fileds
VITAmcg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "RETOLmcg", "CARTBEQmcg"
    )
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        # ! Create a temp row with the number of NAs across the required column
        mutate(temp = rowSums(is.na(dataset %>%
            select(all_of(columns))))) %>%
        rowwise() %>%
        # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
        mutate(VITAmcg_std = ifelse(temp == length(columns), NA, sum(RETOLmcg, (1 / 6 * CARTBEQmcg), na.rm = TRUE))) %>%
        # ! remove the temp column
        select(-temp) %>%
        ungroup(), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
        print("The SOP_std will not be calculated")
    })
}


# Thiamin
# Variable combinations: In absence of THIAmg, use values of THIAHCLmg
# ! THIAmg_std = THIAmg OR THIAHCLmg
THIAmg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("THIAmg", "THIAHCLmg")
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        mutate(THIAmg_std = case_when(
            !is.na(THIAmg) ~ THIAmg,
            is.na(THIAmg) ~ THIAHCLmg
        )), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
    })
}

# # TODO: Function needs testing
# CHOAVLDFg.calculation <- function(PROTg, FATg_standardised, FBGTg, ASHg, ALCg) {
#     ALCg <- ALCg %>% replace_na(0)
#     CHOAVLDFg_std <- 100 - (PROTg + FATg_standardised + FBGTg + ASHg + ALCg)
#     return(CHOAVLDFg_std)
# }
# ! Modified Lucia's function to perform additional checks and throw warnings
# TODO: Create documentation. Function works. Most fileds
CHOAVLDFg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "PROCNTg", "FAT_g_standardised", "FIBTGg", "ASHg_std", "ALCg"
    )
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        # ! I the original function only ALCg had NAs replaced. Is this right then?
        # TODO: future dev should check food group before changing NAs to Zero
        # ! Create a temp row with the number of NAs across the required column
        mutate(temp = rowSums(is.na(dataset %>%
            select(all_of(columns))))) %>%
        rowwise() %>%
        # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
        mutate(CHOAVLDFg_std = ifelse(temp == length(columns), NA, sum(100, -PROCNTg, -FAT_g_standardised, -FIBTGg, -ASHg_std, -ALCg, na.rm = TRUE))) %>%
        # ! remove the temp column
        select(-temp) %>%
        ungroup(), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
        print("The CHOAVLDFg_std will not be calculated")
    })
}

# #! Is this required as well?
# #Working but introduced extra values - Eg. US19 = 818 instead of 808
# nia.conversion <- function(x, var1 = "NIAEQmg", var2 = "TRPmg", var3 = "NIAmg"){
#   if(is.na(var3)){
#   var3 <- as.numeric(var1) - 1/60*as.numeric(var2)
#   }else{var3}
#   print(var3)
#   }
# }
nia_conversion_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("NIAEQmg", "TRPmg", "NIAmg")
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        mutate(nia_conversion_std = case_when(
            !is.na(NIAmg) ~ NIAmg,
            is.na(NIAmg) ~ (NIAEQmg - (1 / 60 * TRPmg))
        )), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
    })
}

# We need to generate a function to recalculate the values of RETOLmcg, when it is not provided. It could be used as well when calculating CARTBEQmcg.
# Retinol (RETOLmcg) can be re-calculated from Vitamin A: [(2*VITA_REAmcg) - VITAmcg] , add comment: "RETOLmcg value re-calulated from VITA_REAmcg and VITAmcg".

RETOLmcg_Recalculator <- function(dataset) {
    # Check presence of required columns
    # TODO: Refactor all code to have the column checker as its own function.
    columns <- c("RETOLmcg", "VITA_RAEmcg", "VITAmcg")
    for (column in columns) {
        if (column %in% names(dataset)) {
        } else {
            stop(paste0(
                "Error: variable ",
                column,
                " not found, halting execution. Please fix your input data and try again"
            ))
        }
    }
    # Try the calculation
    tryCatch(dataset %>%
        as_tibble() %>%
        mutate_at(.vars = columns, .funs = as.numeric) %>%
        mutate(RETOLmcg = case_when(
            is.na(RETOLmcg) ~ (2 * VITA_RAEmcg - VITAmcg),
            !is.na(RETOLmcg) ~ RETOLmcg
        )), error = function(e) {
        print("Error : Required columns not found i.e :")
        print(columns)
        print("The CHOAVLDFg_std will not be calculated")
    })
}
