# =============================================================================
# <environment: namespace:spMisc>
# [!!!] Will be removed in the future
# =============================================================================
getVarValues <-
    function(VAR, DATA,
             CALL = match.call(definition = sys.function(sys.parent()),
                               call = sys.call(sys.parent())),
             env = parent.frame(2L)
) {

    # Prepare data, if needed -------------------------------------------------
    if (inherits(DATA,"hyperSpec")){
        DATA <- DATA$..
    }

    # If VAR is NULL --------------------------------------------------------
    if(is.null(VAR)){return(VAR)}

    # Force evaluation of function arguments ----------------------------------
    force(env) # Get parent environment
    force(CALL)# Get call of function which parameters are going to be evaluated.

    # Look for missing arguments-----------------------------------------------
    missVar <- vector("logical",2)
    missVar[1] <- missing(VAR)
    missVar[2] <- missing(DATA)

    if (any(missVar)) {
        missVarTXT <- paste(c("VAR", "DATA")[missVar],
                            collapse = ", ")
        stop(paste("Missing arguments with no default values:", missVarTXT))
    }
    # -----------------------------------------------------------------------
    VAR_value <- NULL
    try({VAR_value <- VAR}, silent = TRUE)

    # If data is missing (i.e. is NULL) -------------------------------------
    if (is.null(DATA))
        return(VAR)

    # If DATA is provided ---------------------------------------------------
    # and ...
    VAR_length            <- VAR_value  %>% simplify2array  %>% length
    is_VAR_value_in_DATA  <- all(VAR_value %in% colnames(DATA))
    if (VAR_length == 1 & is_VAR_value_in_DATA)
        return(DATA[[VAR_value]])

    #  ------------------------------------------------------------------------
    DATA_length  <- nrow(DATA) %if_null% length(DATA)  # <<<< this line may
    #  need reviewing:
    #  length(data.frame) vs.
    #          length(matrix)
    if (VAR_length == DATA_length)
        return(VAR_value)

    #  ------------------------------------------------------------------------
    # Convert input variable names to character (without evaluation)
    VAR_name   <- CALL[[match.call()$VAR  %>% as.character]] %>% as.character
    is_VAR_name_in_DATA <- VAR_name %in% colnames(DATA)

    if (is_VAR_name_in_DATA)
        return(DATA[[VAR_name]])

    # VAR_value_in_DATA <- env[[DATA_name]][[VAR_name]]
    # VAR_value_in_DATA <- env[[DATA_name]][[,VAR_name,drop=TRUE]]
    # VAR_value %in% colnames(env[[DATA_name]]

    #  ------------------------------------------------------------------------

    warning("Lengths of arguments 'DATA' and 'VAR' do not match!!!")# <<<< this line may need reviewing:
    # Error message is not informative enough

    return(VAR_value)
}
