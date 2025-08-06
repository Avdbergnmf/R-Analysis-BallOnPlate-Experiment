#' Dynamic Input Management Utilities
#'
#' This file contains utilities for managing dynamic selectizeInput choices
#' with intelligent state preservation across data changes.

#' Smart selection function that preserves user choices when possible
#' @param current_value The currently selected value
#' @param new_choices Vector of new available choices
#' @param default_choice Default choice if no previous selection exists
#' @param fallback_choices Vector of preferred choices to try if current not available
#' @param previous_choices Vector of previous choices for position matching (optional)
#' @param use_position_fallback Whether to try position-based fallback if value not found
#' @return The selected value to use
smart_select <- function(current_value, new_choices, default_choice = NULL,
                         fallback_choices = NULL, previous_choices = NULL,
                         use_position_fallback = FALSE) {
    if (is.null(new_choices) || length(new_choices) == 0) {
        return(NULL)
    }

    # If current value exists in new choices, keep it
    if (!is.null(current_value) && current_value %in% new_choices) {
        return(current_value)
    }

    # Try position-based selection if enabled and we have previous choices
    if (use_position_fallback && !is.null(previous_choices) &&
        !is.null(current_value) && current_value %in% previous_choices) {
        index <- match(current_value, previous_choices)
        if (index <= length(new_choices)) {
            return(new_choices[index])
        }
    }

    # Use default choice if specified and available
    if (!is.null(default_choice) && default_choice %in% new_choices) {
        return(default_choice)
    }

    # Try fallback choices in order
    if (!is.null(fallback_choices)) {
        for (fallback in fallback_choices) {
            if (fallback %in% new_choices) {
                return(fallback)
            }
        }
    }

    # Current value was invalid and no fallbacks worked - return NULL
    return(NULL)
}



#' Update a selectizeInput with smart state preservation
#' @param session Shiny session object
#' @param input_id The input ID to update
#' @param choices New choices vector
#' @param selected Currently selected value (will be preserved if possible)
#' @param default_choice Default choice if no previous selection
#' @param fallback_choices Vector of preferred choices to try
#' @param previous_choices Vector of previous choices for first-load detection
#' @param multiple Whether the input supports multiple selection
update_selectize_smart <- function(session, input_id, choices, selected = NULL,
                                   default_choice = NULL, fallback_choices = NULL,
                                   multiple = FALSE, previous_choices = NULL,
                                   use_position_fallback = FALSE) {
    if (multiple) {
        # For multiple selection, preserve all valid selections
        if (!is.null(selected) && length(selected) > 0) {
            valid_selections <- intersect(selected, choices)
            if (length(valid_selections) == 0 && !is.null(fallback_choices)) {
                valid_selections <- intersect(fallback_choices, choices)
            }
            if (length(valid_selections) == 0 && !is.null(default_choice)) {
                valid_selections <- default_choice
            }
            final_selection <- valid_selections
        } else {
            # Handle default choice for multiple selection - use same logic as position function
            first_load <- is.null(previous_choices)
            if (first_load) {
                # First load: apply default, then fallbacks
                if (!is.null(default_choice) && length(intersect(default_choice, choices)) > 0) {
                    final_selection <- intersect(default_choice, choices)
                } else if (!is.null(fallback_choices) && length(intersect(fallback_choices, choices)) > 0) {
                    final_selection <- intersect(fallback_choices, choices)
                } else {
                    final_selection <- character(0)
                }
            } else {
                # Subsequent loads: allow empty selection
                final_selection <- character(0)
            }
        }
    } else {
        final_selection <- smart_select(selected, choices, default_choice, fallback_choices, previous_choices, use_position_fallback)
    }

    # ------------------------------------------------------------------
    # EARLY-EXIT: skip update if both choices and selection are unchanged
    # ------------------------------------------------------------------
    current_sel <- isolate(session$input[[input_id]])
    if (setequal(choices, isolate(session$input[[input_id]] %||% choices)) &&
        setequal(current_sel, final_selection)) {
        return(invisible(NULL))
    }

    # Explicitly pass character(0) when we want an empty selection.
    selected_value <- if (is.null(final_selection)) character(0) else final_selection
    updateSelectizeInput(session, input_id, choices = choices, selected = selected_value)
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
        update_selectize_smart(
            session, input_id, available_cols, current_selection,
            default_choice, fallback_choices, multiple,
            prev_choices,
            use_position_fallback = TRUE
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
    # Track previous category choices to know first load vs subsequent updates
    previous_choices <- reactiveVal(NULL)

    observe({
        data <- data_reactive()

        # Always use the full list when data is unavailable
        if (is.null(data) || nrow(data) == 0) {
            updateSelectizeInput(session, input_id,
                choices = category_choices, selected = NULL
            )
            return()
        }

        # Determine current selection (could be NULL/character(0))
        current_selection <- if (!is.null(input)) {
            isolate(input[[input_id]])
        } else {
            tryCatch(isolate(getDefaultReactiveDomain()$input[[input_id]]),
                error = function(e) NULL
            )
        }

        # Update with position-aware helper (handles first-load default logic)
        update_selectize_smart(
            session, input_id,
            category_choices, current_selection,
            default_selection, NULL, multiple,
            previous_choices(),
            use_position_fallback = TRUE
        )

        # Remember for next round
        previous_choices(category_choices)
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
