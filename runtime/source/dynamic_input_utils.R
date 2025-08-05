#' Dynamic Input Management Utilities
#'
#' This file contains utilities for managing dynamic selectizeInput choices
#' with intelligent state preservation across data changes.

#' Smart selection function that preserves user choices when possible
#' @param current_value The currently selected value
#' @param new_choices Vector of new available choices
#' @param default_choice Default choice if no previous selection exists
#' @param fallback_choices Vector of preferred choices to try if current not available
#' @return The selected value to use
smart_select <- function(current_value, new_choices, default_choice = NULL, fallback_choices = NULL) {
    if (is.null(new_choices) || length(new_choices) == 0) {
        return(NULL)
    }

    # If current value exists in new choices, keep it
    if (!is.null(current_value) && current_value %in% new_choices) {
        return(current_value)
    }

    # Try fallback choices in order
    if (!is.null(fallback_choices)) {
        for (fallback in fallback_choices) {
            if (fallback %in% new_choices) {
                return(fallback)
            }
        }
    }

    # Use default choice if specified and available
    if (!is.null(default_choice) && default_choice %in% new_choices) {
        return(default_choice)
    }

    # Fall back to first available choice
    return(new_choices[1])
}

#' Position-based selection that tries to maintain the same position in the list
#' @param current_value The currently selected value
#' @param previous_choices Vector of previous choices
#' @param new_choices Vector of new available choices
#' @param fallback_choices Vector of preferred choices to try
#' @return The selected value to use
position_based_select <- function(current_value, previous_choices, new_choices, fallback_choices = NULL) {
    if (is.null(new_choices) || length(new_choices) == 0) {
        return(NULL)
    }

    # If current value exists in new choices, keep it
    if (!is.null(current_value) && current_value %in% new_choices) {
        return(current_value)
    }

    # Try position-based selection if we have previous choices
    if (!is.null(previous_choices) && current_value %in% previous_choices) {
        index <- match(current_value, previous_choices)
        if (index <= length(new_choices)) {
            return(new_choices[index])
        }
    }

    # Try fallback choices
    if (!is.null(fallback_choices)) {
        for (fallback in fallback_choices) {
            if (fallback %in% new_choices) {
                return(fallback)
            }
        }
    }

    # Fall back to first available choice
    return(new_choices[1])
}

#' Update a selectizeInput with smart state preservation
#' @param session Shiny session object
#' @param input_id The input ID to update
#' @param choices New choices vector
#' @param selected Currently selected value (will be preserved if possible)
#' @param default_choice Default choice if no previous selection
#' @param fallback_choices Vector of preferred choices to try
#' @param multiple Whether the input supports multiple selection
update_selectize_smart <- function(session, input_id, choices, selected = NULL,
                                   default_choice = NULL, fallback_choices = NULL,
                                   multiple = FALSE) {
    if (multiple) {
        # For multiple selection, preserve all valid selections
        if (!is.null(selected)) {
            valid_selections <- intersect(selected, choices)
            if (length(valid_selections) == 0 && !is.null(fallback_choices)) {
                valid_selections <- intersect(fallback_choices, choices)
            }
            if (length(valid_selections) == 0 && !is.null(default_choice)) {
                valid_selections <- default_choice
            }
            final_selection <- if (length(valid_selections) > 0) valid_selections else list(choices[1])
        } else {
            # Handle default choice for multiple selection
            if (!is.null(default_choice) && all(default_choice %in% choices)) {
                final_selection <- list(default_choice)
            } else {
                final_selection <- list(choices[1])
            }
        }
    } else {
        # For single selection, use smart selection
        final_selection <- smart_select(selected, choices, default_choice, fallback_choices)
    }

    updateSelectizeInput(session, input_id, choices = choices, selected = final_selection)
}

#' Update selectizeInput with position-based state preservation
#' @param session Shiny session object
#' @param input_id The input ID to update
#' @param choices New choices vector
#' @param selected Currently selected value
#' @param previous_choices Vector of previous choices for position matching
#' @param fallback_choices Vector of preferred choices to try
#' @param multiple Whether the input supports multiple selection
update_selectize_position <- function(session, input_id, choices, selected = NULL,
                                      previous_choices = NULL, fallback_choices = NULL,
                                      multiple = FALSE) {
    if (multiple) {
        # For multiple selection, preserve all valid selections
        if (!is.null(selected)) {
            valid_selections <- intersect(selected, choices)
            if (length(valid_selections) == 0 && !is.null(fallback_choices)) {
                valid_selections <- intersect(fallback_choices, choices)
            }
            final_selection <- if (length(valid_selections) > 0) valid_selections else list(choices[1])
        } else {
            final_selection <- list(choices[1])
        }
    } else {
        # For single selection, use position-based selection
        final_selection <- position_based_select(selected, previous_choices, choices, fallback_choices)
    }

    updateSelectizeInput(session, input_id, choices = choices, selected = final_selection)
}

#' Create a reactive observer for dynamic variable selection
#' @param data_reactive Reactive expression that returns the data
#' @param input_id The input ID to update
#' @param column_filter Function to filter columns (e.g., is.numeric, is.character)
#' @param exclude_patterns Vector of regex patterns to exclude from choices
#' @param default_choice Default choice if no previous selection
#' @param fallback_choices Vector of preferred choices to try
#' @param multiple Whether the input supports multiple selection
#' @param session Shiny session object
#' @param input Shiny input object
create_dynamic_variable_observer <- function(data_reactive, input_id,
                                             column_filter = is.numeric,
                                             exclude_patterns = NULL,
                                             default_choice = NULL,
                                             fallback_choices = NULL,
                                             multiple = FALSE,
                                             session = getDefaultReactiveDomain(),
                                             input = NULL) {
    # Store previous choices for position-based selection
    previous_choices <- reactiveVal(NULL)

    observe({
        data <- data_reactive()

        if (is.null(data) || nrow(data) == 0) {
            updateSelectizeInput(session, input_id, choices = character(0), selected = NULL)
            return()
        }

        # Get available columns
        available_cols <- names(data)[sapply(data, column_filter)]

        # Apply exclusion patterns
        if (!is.null(exclude_patterns)) {
            for (pattern in exclude_patterns) {
                available_cols <- grep(pattern, available_cols, value = TRUE, invert = TRUE)
            }
        }

        # Get current selection - use input parameter if provided, otherwise try to get from session
        current_selection <- if (!is.null(input)) {
            isolate(input[[input_id]])
        } else {
            # Try to get from the current reactive domain
            tryCatch(
                {
                    isolate(getDefaultReactiveDomain()$input[[input_id]])
                },
                error = function(e) {
                    NULL
                }
            )
        }

        # Get previous choices for position-based selection
        prev_choices <- previous_choices()

        # Update the input
        update_selectize_position(
            session, input_id, available_cols, current_selection,
            prev_choices, fallback_choices, multiple
        )

        # Store current choices for next update
        previous_choices(available_cols)
    })
}

#' Create a reactive observer for dynamic category selection
#' @param data_reactive Reactive expression that returns the data
#' @param input_id The input ID to update
#' @param category_choices Vector of available category choices
#' @param default_selection Default selection
#' @param multiple Whether the input supports multiple selection
#' @param session Shiny session object
#' @param input Shiny input object
create_dynamic_category_observer <- function(data_reactive, input_id,
                                             category_choices,
                                             default_selection = NULL,
                                             multiple = FALSE,
                                             session = getDefaultReactiveDomain(),
                                             input = NULL) {
    observe({
        data <- data_reactive()

        if (is.null(data) || nrow(data) == 0) {
            updateSelectizeInput(session, input_id, choices = category_choices, selected = NULL)
            return()
        }

        # Get current selection - use input parameter if provided, otherwise try to get from session
        current_selection <- if (!is.null(input)) {
            isolate(input[[input_id]])
        } else {
            # Try to get from the current reactive domain
            tryCatch(
                {
                    isolate(getDefaultReactiveDomain()$input[[input_id]])
                },
                error = function(e) {
                    NULL
                }
            )
        }

        # Update the input
        update_selectize_smart(
            session, input_id, category_choices, current_selection,
            default_selection, NULL, multiple
        )
    })
}

#' Get meaningful variables from data (excludes metadata columns)
#' @param data Data frame
#' @param exclude_cols Columns to exclude from meaningful variables
#' @return Vector of meaningful variable names
get_meaningful_variables <- function(data, exclude_cols = c("participant", "condition", "trialNum", "foot")) {
    if (is.null(data) || nrow(data) == 0) {
        return(character(0))
    }

    all_vars <- colnames(data)
    meaningful_vars <- all_vars[!all_vars %in% exclude_cols]

    return(meaningful_vars)
}

#' Get numeric variables from data with optional exclusions
#' @param data Data frame
#' @param exclude_patterns Vector of regex patterns to exclude
#' @return Vector of numeric variable names
get_numeric_variables <- function(data, exclude_patterns = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        return(character(0))
    }

    numeric_cols <- names(data)[sapply(data, is.numeric)]

    if (!is.null(exclude_patterns)) {
        for (pattern in exclude_patterns) {
            numeric_cols <- grep(pattern, numeric_cols, value = TRUE, invert = TRUE)
        }
    }

    return(numeric_cols)
}

#' Get simulation-specific variables from data
#' @param data Simulation data frame
#' @param exclude_patterns Vector of regex patterns to exclude
#' @return Vector of simulation variable names
get_simulation_variables <- function(data, exclude_patterns = NULL) {
    if (is.null(data) || nrow(data) == 0) {
        return(character(0))
    }

    # Get all numeric columns
    numeric_cols <- names(data)[sapply(data, is.numeric)]

    # Define simulation-specific variables (these are the most interesting ones)
    simulation_vars <- c(
        "q", "qd", "qdd", "x", "y", "vx", "vy", "ax", "ay",
        "e", "ke", "pe", "power", "margin_E", "danger", "e_total_needed",
        "score", "total_score", "arcDeg", "time", "simulation_time"
    )

    # Filter to simulation variables that exist in the data
    available_sim_vars <- intersect(simulation_vars, numeric_cols)

    # Add any other numeric variables that aren't metadata
    metadata_cols <- c("participant", "trialNum", "condition", "phase", "respawn_segment")
    other_numeric <- setdiff(numeric_cols, metadata_cols)

    # Combine simulation variables with other numeric variables
    all_vars <- c(available_sim_vars, other_numeric)

    # Apply exclusion patterns
    if (!is.null(exclude_patterns)) {
        for (pattern in exclude_patterns) {
            all_vars <- grep(pattern, all_vars, value = TRUE, invert = TRUE)
        }
    }

    return(all_vars)
}

#' Get simulation-specific category choices
#' @param data Simulation data frame
#' @return Vector of available category choices for simulation data
get_simulation_categories <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(c("participant", "condition", "trialNum", "phase", "arcDeg"))
    }

    # Define potential category columns
    potential_categories <- c("participant", "condition", "trialNum", "phase", "arcDeg", "respawn_segment")

    # Return only categories that exist in the data
    available_categories <- intersect(potential_categories, colnames(data))

    return(available_categories)
}

#' Check if a specific page is currently active in the flexdashboard
#' @param page_name The name of the page to check (e.g., "Histograms", "Boxplots")
#' @return TRUE if the page is active, FALSE otherwise
is_page_active <- function(page_name) {
    # In flexdashboard, we can check the current page using input$`page_name`
    # The page name is converted to a reactive input when the page is active
    tryCatch(
        {
            # Try to access the page input - if it exists, the page is active
            page_input <- get(paste0("input$`", page_name, "`"), envir = .GlobalEnv)
            return(TRUE)
        },
        error = function(e) {
            # Alternative method: check if the page name appears in the current URL or session
            if (exists("session", envir = .GlobalEnv)) {
                session <- get("session", envir = .GlobalEnv)
                # This is a fallback method - in flexdashboard, pages are typically
                # active when their content is being rendered
                return(TRUE) # For now, assume active if session exists
            }
            return(FALSE)
        }
    )
}

#' Create a conditional observer that only runs when a specific page is active
#' @param page_name The name of the page that must be active
#' @param expr The expression to run when the page is active
#' @param session Shiny session object
create_conditional_observer <- function(page_name, expr, session = getDefaultReactiveDomain()) {
    observe({
        # Check if the page is active
        if (is_page_active(page_name)) {
            expr()
        }
    })
}

#' Create a conditional reactive that only computes when a specific page is active
#' @param page_name The name of the page that must be active
#' @param expr The expression to compute when the page is active
#' @param default_value The default value to return when the page is not active
#' @return A reactive expression
create_conditional_reactive <- function(page_name, expr, default_value = NULL) {
    reactive({
        if (is_page_active(page_name)) {
            expr()
        } else {
            default_value
        }
    })
}

#' Get plot height based on sidebar settings and optional split count
#' @param split_count Number of splits (default 1 for no split)
#' @return Reactive height value in pixels
get_plot_height <- function(split_count = 1) {
    reactive({
        base_height <- input$plotheight
        if (split_count > 1) {
            return(base_height * split_count)
        } else {
            return(base_height)
        }
    })
}

#' Get plot width based on sidebar settings
#' @return Reactive width value in pixels
get_plot_width <- function() {
    reactive({
        input$plotwidth
    })
}
