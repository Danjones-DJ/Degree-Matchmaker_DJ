library(shiny)

# Load the upgraded UCL dataset from GitHub (v5 with university and offer_rate_clean columns)
ucl_data <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/ucl_pcm_degrees_v5.csv", 
                     stringsAsFactors = FALSE)

# Clean and prepare the data
ucl_data$median_salary <- as.numeric(ucl_data$median_salary)
ucl_data$lower_quartile_salary <- as.numeric(ucl_data$lower_quartile_salary)
ucl_data$upper_quartile_salary <- as.numeric(ucl_data$upper_quartile_salary)

# Get dynamic degree types from the actual data
degree_types <- sort(unique(ucl_data$degree_type))

# Load A-Level subjects from GitHub CSV
subjects_data <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/alevel_subjects.csv", 
                          stringsAsFactors = FALSE)
all_subjects <- sort(subjects_data$a_level_subjects)

# Subject synonym mapping for smart matching
create_subject_synonyms <- function() {
  list(
    "Mathematics" = c("Mathematics", "Maths", "Math", "Mathematical", "Further Mathematics", "Statistics"),
    "Physics" = c("Physics", "Physical", "Physical Sciences"),
    "Chemistry" = c("Chemistry", "Chemical", "Chemical Sciences"),
    "Biology" = c("Biology", "Biological", "Biological Sciences", "Life Sciences", "Life and Health Sciences"),
    "English Literature" = c("English Literature", "English", "Literature", "English Language and Literature"),
    "English Language" = c("English Language", "English", "Language", "English Language and Literature"),
    "History" = c("History", "Historical", "Ancient History"),
    "Geography" = c("Geography", "Geographical", "Environmental Geography"),
    "Computer Science" = c("Computer Science", "Computing", "ICT", "Information Technology", "Software Systems Development"),
    "Economics" = c("Economics", "Economic", "Business Economics"),
    "Psychology" = c("Psychology", "Psychological"),
    "Art and Design" = c("Art and Design", "Art", "Design", "Fine Art", "Visual Arts"),
    "Business" = c("Business", "Business Studies", "Commerce"),
    "French" = c("French", "French Language", "French Studies"),
    "German" = c("German", "German Language", "German Studies"),
    "Spanish" = c("Spanish", "Spanish Language", "Spanish Studies"),
    "Politics" = c("Politics", "Political Science", "Government", "Government and Politics"),
    "Philosophy" = c("Philosophy", "Philosophical"),
    "Sociology" = c("Sociology", "Social Sciences", "Sociological"),
    "Drama" = c("Drama", "Theatre", "Drama and Theatre", "Performing Arts"),
    "Music" = c("Music", "Musical", "Music Technology"),
    "Physical Education" = c("Physical Education", "PE", "Sports", "Sports Science"),
    "Religious Studies" = c("Religious Studies", "Religion", "Theology", "Islamic Studies", "Biblical Studies"),
    "Media Studies" = c("Media Studies", "Media", "Film Studies", "Digital Media and Design"),
    "Law" = c("Law", "Legal Studies", "Jurisprudence")
  )
}

# Function to match subjects with requirements using synonyms
match_subjects_with_requirements <- function(selected_subjects, course_requirements) {
  if(is.null(selected_subjects) || length(selected_subjects) == 0 || 
     is.null(course_requirements) || is.na(course_requirements) || course_requirements == "") {
    return(FALSE)
  }
  
  synonyms <- create_subject_synonyms()
  
  # Create expanded list of all possible subject variations
  all_variations <- c()
  for(subject in selected_subjects) {
    if(subject %in% names(synonyms)) {
      all_variations <- c(all_variations, synonyms[[subject]])
    } else {
      all_variations <- c(all_variations, subject)
    }
  }
  
  # Check if any variation appears in the requirements
  any(sapply(all_variations, function(var) {
    grepl(var, course_requirements, ignore.case = TRUE)
  }))
}

# User's working matching function - implemented exactly as provided
find_matched_subjects <- function(a_levels, subject_requirements_example) {
  # Handle null/empty inputs
  if(is.null(a_levels) || length(a_levels) == 0 || 
     is.null(subject_requirements_example) || is.na(subject_requirements_example) || 
     subject_requirements_example == "") {
    return(character(0))
  }
  
  matched_subjects <- c()
  
  for (subject in a_levels) {
    if (grepl(subject, subject_requirements_example, ignore.case = TRUE)) {
      matched_subjects <- c(matched_subjects, subject)
      cat("match", subject, "\n")
    }
  }
  
  return(matched_subjects)
}

# Function to get student's selected subjects
get_selected_subjects <- function(subject1, subject2, subject3, subject4) {
  subjects <- c(subject1, subject2, subject3, subject4)
  subjects <- subjects[!is.null(subjects) & subjects != ""]
  return(subjects)
}

# Grade scoring function - UPDATED VALUES
grade_to_score <- function(grade) {
  grade_map <- c("A*" = 100, "A" = 30, "B" = 10, "C" = 3, "D" = 1, "E" = 0, "U" = 0)
  return(grade_map[grade])
}

# Function to convert grade requirements to scores - FIXED A* PARSING
convert_grade_requirement <- function(grade_req) {
  if(is.na(grade_req) || grade_req == "") return(0)
  
  # Handle A* grades more carefully
  # Split by A* first, then handle remaining
  parts <- strsplit(grade_req, "A\\*")[[1]]
  
  # Count A* grades
  num_a_star <- length(parts) - 1
  
  # Get remaining grades from the last part
  remaining <- parts[length(parts)]
  remaining_grades <- if(remaining == "") character(0) else unlist(strsplit(remaining, ""))
  remaining_grades <- remaining_grades[remaining_grades %in% c("A", "B", "C", "D", "E", "U")]
  
  # Combine all grades
  all_grades <- c(rep("A*", num_a_star), remaining_grades)
  
  if(length(all_grades) == 0) return(0)
  
  # Calculate total score
  scores <- sapply(all_grades, grade_to_score, USE.NAMES = FALSE)
  return(sum(scores))
}

# Simple similar courses finder - back to basics with your requirements
find_similar_courses <- function(current_course, all_courses, limit = 3) {
  # Remove the current course itself from consideration
  all_courses <- all_courses[all_courses$title != current_course$title, ]
  
  if(nrow(all_courses) == 0) return(data.frame())
  
  # OVERARCHING FILTER: Only courses with same or lower grade requirements
  if(!is.na(current_course$grade_score)) {
    all_courses <- all_courses[is.na(all_courses$grade_score) | all_courses$grade_score <= current_course$grade_score, ]
    cat("After grade filtering (same or lower):", nrow(all_courses), "courses remaining\n")
  }
  
  if(nrow(all_courses) == 0) return(data.frame())
  
  similar_courses <- data.frame()
  
  # FILTER 1: Name similarity including compound terms
  cat("FILTER 1: Checking name similarity for:", current_course$title, "\n")
  
  name_similar <- data.frame()
  for(i in 1:nrow(all_courses)) {
    course_title <- tolower(all_courses$title[i])
    current_title <- tolower(current_course$title)
    
    # Check for compound science terms first
    compound_matches <- FALSE
    science_compounds <- c("data science", "social sciences", "computer science", "political science", 
                           "life sciences", "physical sciences", "natural sciences", "environmental sciences")
    
    for(compound in science_compounds) {
      if(grepl(compound, current_title) && grepl(compound, course_title)) {
        compound_matches <- TRUE
        cat("Compound match found:", all_courses$title[i], "- compound:", compound, "\n")
        break
      }
    }
    
    # Check for individual word matches (excluding standalone "science")
    word_matches <- FALSE
    if(!compound_matches) {
      current_words <- strsplit(gsub("[^a-z ]", " ", current_title), "\\s+")[[1]]
      current_words <- current_words[!current_words %in% c("", "and", "with", "the", "of", "in", "for", "to", "bsc", "ba", "msc", "ma", "meng", "beng", "science", "sciences")]
      current_words <- current_words[nchar(current_words) > 2]
      
      course_words <- strsplit(gsub("[^a-z ]", " ", course_title), "\\s+")[[1]]
      course_words <- course_words[!course_words %in% c("", "and", "with", "the", "of", "in", "for", "to", "bsc", "ba", "msc", "ma", "meng", "beng", "science", "sciences")]
      course_words <- course_words[nchar(course_words) > 2]
      
      overlap <- intersect(current_words, course_words)
      if(length(overlap) >= 1) {
        word_matches <- TRUE
        cat("Word match found:", all_courses$title[i], "- words:", paste(overlap, collapse = ", "), "\n")
      }
    }
    
    if(compound_matches || word_matches) {
      name_similar <- rbind(name_similar, all_courses[i, ])
    }
  }
  
  # Add name similar courses to results (up to limit)
  if(nrow(name_similar) > 0) {
    # Sort by grade requirement similarity
    if(!is.na(current_course$grade_score)) {
      name_similar$score_diff <- abs(name_similar$grade_score - current_course$grade_score)
      name_similar <- name_similar[order(name_similar$score_diff), ]
      name_similar$score_diff <- NULL
    }
    
    to_add <- min(nrow(name_similar), limit - nrow(similar_courses))
    similar_courses <- rbind(similar_courses, head(name_similar, to_add))
    cat("Added", to_add, "courses from Filter 1\n")
  }
  
  # FILTER 2: Exact same category tags (only if we need more)
  if(nrow(similar_courses) < limit) {
    cat("FILTER 2: Checking exact category matches\n")
    
    # Get categories for current course
    current_categories <- c()
    categories <- c("Natural Sciences", "Humanities", "Architecture", "Computational & Mathematical Sciences",
                    "Social Sciences", "Management", "Medicine", "Sustainability", "Engineering", 
                    "Languages", "Arts", "Education", "Technology", "Law")
    
    for(category in categories) {
      category_courses <- get_subject_category_courses(category, all_courses)
      if(current_course$title %in% c(category_courses$title, current_course$title)) {
        current_categories <- c(current_categories, category)
      }
    }
    
    if(length(current_categories) > 0) {
      exact_category_matches <- data.frame()
      
      for(i in 1:nrow(all_courses)) {
        # Skip if already added
        if(all_courses$title[i] %in% similar_courses$title) next
        
        # Check if this course matches ALL categories
        course_categories <- c()
        for(category in categories) {
          category_courses <- get_subject_category_courses(category, all_courses)
          if(all_courses$title[i] %in% category_courses$title) {
            course_categories <- c(course_categories, category)
          }
        }
        
        # Must match ALL categories exactly
        if(length(course_categories) == length(current_categories) && 
           all(current_categories %in% course_categories)) {
          exact_category_matches <- rbind(exact_category_matches, all_courses[i, ])
          cat("Exact category match found:", all_courses$title[i], "\n")
        }
      }
      
      # Add exact category matches
      if(nrow(exact_category_matches) > 0) {
        if(!is.na(current_course$grade_score)) {
          exact_category_matches$score_diff <- abs(exact_category_matches$grade_score - current_course$grade_score)
          exact_category_matches <- exact_category_matches[order(exact_category_matches$score_diff), ]
          exact_category_matches$score_diff <- NULL
        }
        
        to_add <- min(nrow(exact_category_matches), limit - nrow(similar_courses))
        similar_courses <- rbind(similar_courses, head(exact_category_matches, to_add))
        cat("Added", to_add, "courses from Filter 2\n")
      }
    }
  }
  
  # FILTER 3: Randomization (only if we still need more)
  if(nrow(similar_courses) < limit) {
    cat("FILTER 3: Using randomization to fill remaining slots\n")
    
    remaining_courses <- all_courses[!all_courses$title %in% similar_courses$title, ]
    
    if(nrow(remaining_courses) > 0) {
      needed <- limit - nrow(similar_courses)
      random_indices <- sample(1:nrow(remaining_courses), min(needed, nrow(remaining_courses)))
      random_courses <- remaining_courses[random_indices, ]
      
      similar_courses <- rbind(similar_courses, random_courses)
      cat("Added", nrow(random_courses), "courses from Filter 3 (random)\n")
    }
  }
  
  cat("Final similar courses count:", nrow(similar_courses), "\n")
  return(head(similar_courses, limit))
}
calculate_student_score <- function(grades) {
  # Remove empty grades
  valid_grades <- grades[grades != "" & !is.na(grades)]
  
  # Must have at least 3 grades
  if(length(valid_grades) < 3) return(NA)
  
  # Convert to scores and take best 3
  scores <- sapply(valid_grades, grade_to_score, USE.NAMES = FALSE)
  best_3_scores <- sort(scores, decreasing = TRUE)[1:3]
  
  return(sum(best_3_scores))
}

# Function to determine match type - FIXED LOGIC
get_match_type <- function(student_score, course_score) {
  if(is.na(student_score) || is.na(course_score)) return("No Data")
  
  if(student_score == course_score) return("Exact Match")
  else if(student_score > course_score) {
    difference <- student_score - course_score
    if(difference <= 30) return("Good Match")  # Within ~1 grade difference
    else return("Overmatch")
  }
  else return("No Match")  # student_score < course_score means not qualified
}

# Function to calculate student's best 3 grades score
calculate_student_score <- function(grades) {
  # Remove empty grades
  valid_grades <- grades[grades != "" & !is.na(grades)]
  
  # Must have at least 3 grades
  if(length(valid_grades) < 3) return(NA)
  
  # Convert to scores and take best 3
  scores <- sapply(valid_grades, grade_to_score, USE.NAMES = FALSE)
  best_3_scores <- sort(scores, decreasing = TRUE)[1:3]
  
  return(sum(best_3_scores))
}

# Add grade scores to dataset - RECALCULATE WITH FIXED FUNCTION
ucl_data$grade_score <- sapply(ucl_data$a_level, convert_grade_requirement)

# Sort dataset by grade requirement (highest first)
ucl_data <- ucl_data[order(-ucl_data$grade_score, na.last = TRUE), ]

# Subject categories mapping (full mapping with all 14 categories)
get_subject_category_courses <- function(category, data) {
  if(category == "Natural Sciences") {
    keywords <- c("Applied Medical Sciences", "Audiology", "Biochemistry", "Biological Sciences", 
                  "Biomedical Sciences", "Bioprocessing", "Business and Health", "Cancer Biomedicine",
                  "Chemistry", "Earth Sciences", "Environmental Geoscience", "Geography and Economics",
                  "Geography", "Geology", "Human Neuroscience", "Human Sciences", "Infection and Immunity",
                  "Mathematics with Mathematical Physics", "Mathematics and Physics", "Neuroscience",
                  "Nutrition and Medical Sciences", "Population Health Sciences", "Psychology",
                  "Science and Engineering", "Sport and Exercise Medical Sciences", "Sustainable Built",
                  "Theoretical Physics", "Biochemical Engineering", "Biomedical Engineering")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Humanities") {
    keywords <- c("Anthropology", "Archaeology", "Experimental Linguistics", "Global Humanitarian",
                  "History and Philosophy", "Philosophy, Politics and Economics", "Politics and International",
                  "Urban Planning", "Urban Studies", "Ancient History", "Classical", "Classics",
                  "Comparative Literature", "Creative Arts and Humanities", "Education, Society",
                  "History", "Philosophy", "Politics, Sociology", "Viking", "Bulgarian", "Czech",
                  "Finnish", "Hungarian", "Polish", "Romanian", "Russian and History", "Ukrainian", "Serbian", "Croatian")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Architecture") {
    keywords <- c("Architectural", "Architecture")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Computational & Mathematical Sciences") {
    keywords <- c("Astrophysics", "Computer Science", "Crime and Security Science", "Data Science",
                  "Geophysics", "Mathematics", "Statistical Science", "Statistics", "Physics",
                  "Electronic and Electrical Engineering", "Mechanical Engineering", "Philosophy and Computer")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Social Sciences") {
    keywords <- c("Social Sciences", "Geography", "Economics", "Politics", "Sociology", "European Social")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Management") {
    keywords <- c("Management", "Business")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Medicine") {
    keywords <- c("Medical", "Medicine", "Biomedical", "Cancer", "Neuroscience", "Pharmacology", "Sport and Exercise")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Sustainability") {
    keywords <- c("Sustainable", "Sustainability")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Engineering") {
    keywords <- c("Engineering", "Computer Science")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Languages") {
    languages <- c("Dutch", "French", "German", "Hebrew", "Hungarian", "Italian", "Norwegian", 
                   "Polish", "Romanian", "Russian Studies", "Scandinavian Studies", 
                   "Spanish and Latin American Studies", "Bulgarian", "Czech", "Danish", 
                   "Finnish", "Serbian", "Croatian", "Swedish", "Ukrainian", "Ancient Languages",
                   "Linguistics", "Psychology and Language Sciences")
    pattern <- paste(languages, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Arts") {
    keywords <- c("Fine Art", "Art", "History of Art", "Media", "Creative Arts", "English", "Literature")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Education") {
    keywords <- c("Education", "Early Childhood")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Technology") {
    keywords <- c("Information Management", "Art and Technology", "Electronic and Electrical")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Law") {
    keywords <- c("Law", "Laws")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
  }
  
  return(data)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      * {
        box-sizing: border-box;
      }
      
      body, html {
        margin: 0 !important;
        padding: 0 !important;
        background: linear-gradient(135deg, #1a1a1a 0%, #2d2d2d 100%) !important;
        color: white !important;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
        height: auto !important;
        min-height: auto !important;
        max-height: none !important;
        overflow: visible !important;
      }
      
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        width: 100% !important;
        height: auto !important;
        min-height: auto !important;
        max-height: none !important;
        overflow: visible !important;
        background: linear-gradient(135deg, #1a1a1a 0%, #2d2d2d 100%) !important;
      }
      
      .main-header {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        color: #1a1a1a;
        padding: 25px;
        text-align: center;
        font-size: 28px;
        font-weight: 700;
        width: 100%;
        box-shadow: 0 4px 20px rgba(157, 197, 220, 0.3);
      }
      
      .content-wrapper {
        padding: 20px;
        width: 100%;
        background: transparent;
      }
      
      .section-title {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 12px;
        color: white;
      }
      
      .grade-section-vertical {
        display: flex;
        flex-direction: column;
        gap: 15px;
        margin-bottom: 30px;
      }
      
      .grade-pair {
        display: flex;
        gap: 10px;
      }
      
      .grade-card-left {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 25px;
        padding: 5px 10px;
        text-align: center;
        font-size: 16px;
        font-weight: 500;
        min-height: 50px;
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 2;
        border: 1px solid #444;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
      }
      
      .grade-card-right {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 25px;
        padding: 5px 10px;
        text-align: center;
        font-size: 16px;
        font-weight: 500;
        min-height: 50px;
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 1;
        border: 1px solid #444;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
      }
      
      .grade-card-left select, .grade-card-right select {
        background: transparent;
        border: none;
        color: white;
        font-size: 16px;
        font-weight: 500;
        width: 100%;
        text-align: center;
        outline: none;
      }
      
      .grade-card-left select option, .grade-card-right select option {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        color: white;
      }
      
      /* NEW: Subject-Grade Pair Styling - FIXED INTERFERENCE */
      .subject-grade-pair {
        transition: all 0.3s ease;
        position: relative;
        z-index: 1;
      }
      
      .subject-grade-pair:hover {
        transform: translateY(-1px);
        z-index: 10;
      }
      
      .subject-card select, .grade-card select {
        background: transparent !important;
        border: none !important;
        color: white !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        width: 100% !important;
        text-align: center !important;
        outline: none !important;
        padding: 4px !important;
        border-radius: 6px !important;
        height: 28px !important;
        position: relative !important;
        z-index: 100 !important;
      }
      
      /* Fix select dropdown z-index issues and prevent interference */
      .subject-card select:focus, .grade-card select:focus {
        z-index: 1000 !important;
        position: relative !important;
        background: rgba(45, 45, 45, 0.95) !important;
      }
      
      .subject-card select, .grade-card select {
        background: transparent !important;
        border: none !important;
        color: white !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        width: 100% !important;
        text-align: center !important;
        outline: none !important;
        padding: 4px !important;
        border-radius: 6px !important;
        height: 28px !important;
        position: relative !important;
        z-index: 100 !important;
      }
      
      /* Prevent hover interference between select elements */
      .subject-card:hover select, .grade-card:hover select {
        background: rgba(157, 197, 220, 0.1) !important;
      }
      
      /* Ensure select options have proper styling */
      .subject-card select option, .grade-card select option {
        background: #1a1a1a !important;
        color: white !important;
        padding: 6px !important;
        font-size: 12px !important;
        z-index: 1000 !important;
        border: none !important;
        outline: none !important;
      }
      
      .subject-card select:focus, .grade-card select:focus {
        box-shadow: 0 0 0 2px rgba(157, 197, 220, 0.3) !important;
        background: rgba(45, 45, 45, 0.95) !important;
      }
      
      /* Subject card specific styling */
      .subject-card {
        transition: all 0.3s ease;
        position: relative;
        z-index: 1;
      }
      
      .subject-card:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3) !important;
        z-index: 10;
      }
      
      /* Grade card specific styling */
      .grade-card {
        transition: all 0.3s ease;
        position: relative;
        z-index: 1;
      }
      
      .grade-card:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3) !important;
        z-index: 10;
      }
      
      /* Add subtle animations */
      .grade-card select {
        transition: all 0.2s ease;
      }
      
      .grade-card select:hover {
        background: rgba(157, 197, 220, 0.1) !important;
      }
      
      /* FIXED: Subject Interest Card Styles - Prevent click-through */
      .subject-interest-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 8px;
        padding: 8px;
        text-align: center;
        font-size: 12px;
        min-height: 35px;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        border: 1px solid #444;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
        position: relative;
        z-index: 10;
        pointer-events: auto;
      }
      
      .subject-interest-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.2);
      }
      
      .subject-interest-card.selected {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%) !important;
        color: #1a1a1a !important;
        font-weight: 600;
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.4);
      }
      
      /* FIXED: Degree Type Card Styles - Consistent with old design */
      .degree-type-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        color: white;
        border-radius: 8px;
        padding: 8px 4px;
        text-align: center;
        font-size: 11px;
        min-height: 30px;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        border: 1px solid #444;
        position: relative;
        z-index: 10;
        pointer-events: auto;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 500;
      }
      
      .degree-type-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.2);
      }
      
      .degree-type-card.selected {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%) !important;
        color: #1a1a1a !important;
        font-weight: 600;
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.4);
      }
      
      .degree-type-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.2);
      }
      
      .degree-type-card.selected {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%) !important;
        color: #1a1a1a !important;
        font-weight: 600;
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.4);
      }
      
      .interest-grid-2x4 {
        display: flex;
        gap: 15px;
        margin-bottom: 30px;
      }
      
      .interest-column {
        flex: 1;
        display: flex;
        flex-direction: column;
        gap: 15px;
      }
      
      .interest-card {
        background-color: #6a6a6a;
        border-radius: 25px;
        padding: 15px 20px;
        text-align: center;
        font-size: 16px;
        min-height: 50px;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        user-select: none;
      }
      
      .interest-card.selected {
        background-color: #8a8a8a;
      }
      
      .control-buttons-top {
        margin-bottom: 20px;
      }
      
      .control-btn-wide {
        background-color: #666666;
        color: white;
        border: none;
        border-radius: 25px;
        padding: 12px 20px;
        margin: 5px 0;
        cursor: pointer;
        font-size: 14px;
        width: 100%;
        display: block;
      }
      
      .control-btn-wide:hover {
        background-color: #777777;
      }
      
      .degree-section-compact {
        background-color: #3a3a3a;
        border-radius: 15px;
        padding: 20px;
      }
      
      .degree-grid-3x3 {
        display: grid;
        grid-template-columns: 1fr 1fr;
        grid-template-rows: 1fr 1fr 1fr 1fr;
        gap: 8px;
      }
      
      .degree-btn-small {
        background-color: #777777;
        color: white;
        border: none;
        border-radius: 15px;
        padding: 8px 12px;
        cursor: pointer;
        font-size: 12px;
        text-align: center;
        user-select: none;
      }
      
      .degree-btn-small:hover {
        background-color: #888888;
      }
      
      .degree-btn-small.selected {
        background-color: #999999;
      }
      
      .submit-btn {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        color: #1a1a1a;
        border: none;
        border-radius: 25px;
        padding: 15px 30px;
        margin: 20px auto;
        cursor: pointer;
        font-size: 16px;
        font-weight: 700;
        display: block;
        min-width: 200px;
        transition: all 0.3s ease;
        box-shadow: 0 6px 20px rgba(157, 197, 220, 0.3);
      }
      
      .submit-btn:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(157, 197, 220, 0.4);
      }
      
      .filter-section {
        margin: 40px 0;
        padding: 20px 0;
        max-width: 1400px;
        margin-left: auto;
        margin-right: auto;
        padding-left: 20px;
        padding-right: 20px;
      }
      
      .filter-btn {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        color: white;
        border: 1px solid #444;
        border-radius: 25px;
        padding: 12px 24px;
        margin: 8px;
        cursor: pointer;
        font-size: 14px;
        font-weight: 600;
        transition: all 0.3s ease;
      }
      
      .filter-btn.active {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        color: #1a1a1a;
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.3);
      }
      
      .filter-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.2);
      }
      
      .courses-section {
        margin: 40px 0;
        padding: 20px 0;
        width: 100%;
      }
      
      .courses-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 40px;
        width: 100%;
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 20px;
      }
      
      .course-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 15px;
        padding: 25px;
        min-height: 150px;
        width: 100%;
        min-width: 600px;
        border: 1px solid #444;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.3);
      }
      
      .course-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(157, 197, 220, 0.2);
      }
      
      .course-title {
        font-size: 18px;
        font-weight: 700;
        margin-bottom: 12px;
        color: #9dc5dc;
        cursor: pointer;
        transition: color 0.3s ease;
      }
      
      .course-title:hover {
        color: #7ab8d3;
      }
      
      .course-details {
        font-size: 14px;
        color: #ccc;
        margin-bottom: 15px;
      }
      
      .course-btn {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        color: #1a1a1a;
        border: none;
        border-radius: 20px;
        padding: 10px 18px;
        margin: 5px 8px 5px 0;
        cursor: pointer;
        font-size: 13px;
        font-weight: 600;
        transition: all 0.3s ease;
      }
      
      .course-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.3);
      }
      
      .recommended-section {
        margin: 50px 0;
        padding: 30px 0;
        border-top: 1px solid #555;
      }
      
      .recommended-grid {
        display: flex;
        gap: 20px;
        overflow-x: auto;
        padding: 20px 0;
      }
      
      .recommended-card {
        background-color: #555555;
        border-radius: 12px;
        padding: 18px;
        min-width: 200px;
        flex-shrink: 0;
      }
      
      .recommended-image {
        background-color: #777777;
        height: 120px;
        border-radius: 8px;
        margin-bottom: 12px;
      }
      
      .recommended-title {
        font-size: 14px;
        font-weight: bold;
        line-height: 1.4;
        color: white;
      }
      
      .custom-modal {
        display: none;
        position: fixed;
        z-index: 1000;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.8);
        padding: 20px;
        box-sizing: border-box;
      }
      
      .modal-content-custom {
        background: linear-gradient(135deg, #1a1a1a 0%, #2d2d2d 100%);
        border-radius: 20px;
        max-width: 1200px;
        margin: 0 auto;
        padding: 30px;
        color: white;
        position: relative;
        max-height: 90vh;
        overflow-y: auto;
        box-shadow: 0 25px 50px rgba(0, 0, 0, 0.7);
      }
      
      .modal-close {
        position: absolute;
        top: 15px;
        right: 20px;
        font-size: 28px;
        font-weight: bold;
        cursor: pointer;
        color: white;
      }
      
      .modal-close:hover {
        opacity: 0.7;
      }
      
      .course-detail-header {
        display: flex;
        gap: 20px;
        margin-bottom: 30px;
        padding: 20px;
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        border-radius: 15px;
        align-items: center;
        box-shadow: 0 8px 25px rgba(157, 197, 220, 0.3);
      }
      
      .header-item {
        color: #1a1a1a;
        font-weight: 600;
        font-size: 16px;
        text-align: center;
        flex: 1;
      }
      
      .header-title {
        flex: 2;
        font-size: 24px;
        font-weight: 700;
        color: #1a1a1a;
      }
      
      .match-level-section {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 25px;
        border-left: 4px solid #9dc5dc;
      }
      
      .match-level-text {
        font-size: 18px;
        font-weight: 600;
        color: white;
      }
      
      .stats-row {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr;
        gap: 20px;
        margin-bottom: 25px;
      }
      
      .stat-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 12px;
        padding: 20px;
        text-align: center;
        border: 1px solid #444;
      }
      
      .stat-label {
        font-size: 12px;
        color: #9dc5dc;
        text-transform: uppercase;
        font-weight: 600;
        margin-bottom: 8px;
      }
      
      .stat-value {
        font-size: 20px;
        font-weight: 700;
        color: white;
      }
      
      .modal-two-columns {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        margin-top: 20px;
      }
      
      .chart-section {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 15px;
        padding: 25px;
        text-align: center;
        color: #ccc;
        min-height: 250px;
        display: flex;
        align-items: center;
        justify-content: center;
        border: 1px solid #444;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
      }
      
      .features-section {
        display: flex;
        flex-direction: column;
        gap: 20px;
      }
      
      .features-title {
        color: #9dc5dc;
        font-size: 18px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .features-toggle {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        border-radius: 12px;
        padding: 15px 25px;
        color: #1a1a1a;
        text-align: center;
        cursor: pointer;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.3);
      }
      
      .features-toggle:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(157, 197, 220, 0.4);
      }
      
      .features-options {
        display: none;
      }
      
      .university-btn {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        border: none;
        border-radius: 12px;
        padding: 15px 30px;
        color: #1a1a1a;
        font-weight: 600;
        cursor: pointer;
        width: 100%;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.3);
      }
      
      .university-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(157, 197, 220, 0.4);
      }
      
      .similar-degrees {
        margin-top: 15px;
      }
      
      .similar-degree-card {
        display: grid;
        grid-template-columns: 2fr 1fr 1fr;
        gap: 15px;
        margin-bottom: 15px;
        align-items: center;
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 12px;
        padding: 15px;
        border: 1px solid #444;
        transition: all 0.3s ease;
      }
      
      .similar-degree-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3);
      }
      
      .similar-degree-info {
        color: white;
      }
      
      .similar-degree-title {
        font-weight: 600;
        font-size: 16px;
        margin-bottom: 5px;
        color: #9dc5dc;
      }
      
      .similar-degree-details {
        font-size: 13px;
        color: #ccc;
      }
      
      .similar-degree-btn {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        border: none;
        border-radius: 8px;
        padding: 8px 15px;
        color: #1a1a1a;
        font-size: 12px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .similar-degree-btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.3);
      }
      
      .go-back-btn {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%);
        border: none;
        border-radius: 10px;
        padding: 10px 20px;
        color: #1a1a1a;
        margin-bottom: 25px;
        cursor: pointer;
        font-weight: 600;
        transition: all 0.3s ease;
      }
      
      .go-back-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(157, 197, 220, 0.3);
      }
      
      .row {
        margin: 0 !important;
      }
      
     .col-sm-6 {
        padding: 10px !important;
      }
      
      /* IMPROVED TOP SECTION STYLING - HARMONIZED DESIGN */
      
      /* Main container with better spacing and visual separation */
      .top-section-container {
        padding: 40px 60px;
        background: linear-gradient(135deg, #3a3a3a 0%, #454545 100%);
        margin: 30px 40px;
        border-radius: 25px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
        border: 1px solid #555;
      }
      
      /* Column styling with better spacing */
      .section-column {
        padding: 0 20px;
      }
      
      /* Section titles - more prominent and consistent */
      .section-title-enhanced {
        font-size: 22px;
        font-weight: 700;
        margin-bottom: 15px;
        color: #9dc5dc;
        text-transform: uppercase;
        letter-spacing: 1px;
        text-align: center;
        padding-bottom: 10px;
        border-bottom: 2px solid rgba(157, 197, 220, 0.3);
      }
      
      /* GRADES & SUBJECTS SECTION - Enhanced subject-grade pairs */
      .enhanced-subject-grade-container {
        display: flex;
        flex-direction: column;
        gap: 12px;
        max-width: 380px;
        margin: 0 auto;
      }
      
      .enhanced-subject-grade-pair {
        display: flex;
        gap: 12px;
        transition: all 0.3s ease;
        position: relative;
        z-index: 1;
      }
      
      .enhanced-subject-grade-pair:hover {
        transform: translateY(-2px);
        z-index: 10;
      }
      
      /* Enhanced subject card - wider and more prominent */
      .enhanced-subject-card {
        flex: 2.5;
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 12px;
        padding: 12px 16px;
        border: 1px solid #555;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.25);
        transition: all 0.3s ease;
        min-height: 60px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      
      .enhanced-subject-card:hover {
        border-color: #9dc5dc;
        box-shadow: 0 6px 20px rgba(157, 197, 220, 0.2);
      }
      
      /* Enhanced grade card - consistent with subject cards */
      .enhanced-grade-card {
        flex: 1;
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 12px;
        padding: 12px 16px;
        border: 1px solid #555;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.25);
        transition: all 0.3s ease;
        min-height: 60px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      
      .enhanced-grade-card:hover {
        border-color: #9dc5dc;
        box-shadow: 0 6px 20px rgba(157, 197, 220, 0.2);
      }
      
      /* Enhanced labels */
      .enhanced-field-label {
        color: #9dc5dc;
        font-size: 10px;
        font-weight: 700;
        margin-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      /* Enhanced select inputs */
      .enhanced-subject-card select,
      .enhanced-grade-card select {
        background: transparent !important;
        border: none !important;
        color: white !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        width: 100% !important;
        text-align: center !important;
        outline: none !important;
        padding: 6px !important;
        border-radius: 8px !important;
        height: auto !important;
        position: relative !important;
        z-index: 100 !important;
      }
      
      .enhanced-subject-card select:focus,
      .enhanced-grade-card select:focus {
        background: rgba(157, 197, 220, 0.1) !important;
        box-shadow: 0 0 0 2px rgba(157, 197, 220, 0.3) !important;
      }
      
      /* Optional field styling */
      .optional-field {
        opacity: 0.75;
      }
      
      .optional-field .enhanced-subject-card,
      .optional-field .enhanced-grade-card {
        background: linear-gradient(135deg, #252525 0%, #323232 100%);
        border-color: #666;
      }
      
      .optional-field .enhanced-field-label {
        color: #8db4c9;
      }
      
      /* SUBJECTS OF INTEREST SECTION - Enhanced cards */
      .enhanced-interest-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 12px;
        max-width: 480px;
        margin: 0 auto;
      }
      
      .enhanced-interest-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 12px;
        padding: 16px 12px;
        text-align: center;
        font-size: 13px;
        font-weight: 600;
        min-height: 60px;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        border: 1px solid #555;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.25);
        position: relative;
        z-index: 10;
        pointer-events: auto;
        line-height: 1.3;
      }
      
      .enhanced-interest-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 20px rgba(157, 197, 220, 0.2);
        border-color: #9dc5dc;
      }
      
      .enhanced-interest-card.selected {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%) !important;
        color: #1a1a1a !important;
        font-weight: 700;
        box-shadow: 0 8px 25px rgba(157, 197, 220, 0.4);
        border-color: #9dc5dc;
      }
      
      /* DEGREE TYPES SECTION - Significantly enhanced */
      .enhanced-degree-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 12px;
        max-width: 320px;
        margin: 0 auto;
      }
      
      .enhanced-degree-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        color: white;
        border-radius: 12px;
        padding: 16px 8px;
        text-align: center;
        font-size: 13px;
        font-weight: 600;
        min-height: 60px;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        border: 1px solid #555;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.25);
        position: relative;
        z-index: 10;
        pointer-events: auto;
        display: flex;
        align-items: center;
        justify-content: center;
        line-height: 1.2;
      }
      
      .enhanced-degree-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 20px rgba(157, 197, 220, 0.2);
        border-color: #9dc5dc;
      }
      
      .enhanced-degree-card.selected {
        background: linear-gradient(135deg, #9dc5dc 0%, #7ab8d3 100%) !important;
        color: #1a1a1a !important;
        font-weight: 700;
        box-shadow: 0 8px 25px rgba(157, 197, 220, 0.4);
        border-color: #9dc5dc;
      }
      
      /* Small text for longer degree names */
      .enhanced-degree-card.small-text {
        font-size: 11px;
        padding: 12px 6px;
      }
      
      /* Visual separators between columns */
      .column-separator {
        width: 1px;
        background: linear-gradient(to bottom, transparent, rgba(157, 197, 220, 0.3), transparent);
        margin: 20px 0;
      }
    "))
  ),
  
  # Header
  div(class = "main-header", "Course Grade Tracker"),
  
  # Main content wrapper
  div(class = "content-wrapper",
      # Enhanced Top section with harmonized design
      div(class = "top-section-container",
          fluidRow(style = "align-items: flex-start; margin: 0;",
                   
                   # Column 1: Your Grades & Subjects (Enhanced)
                   column(4, class = "section-column",
                          div(class = "section-title-enhanced", "Your Grades & Subjects"),
                          div(style = "color: #8db4c9; font-size: 12px; margin-bottom: 20px; text-align: center; font-weight: 500;", 
                              "Select your A-Level subjects and grades (3 required)"),
                          
                          div(class = "enhanced-subject-grade-container",
                              # Enhanced Subject-Grade Pair 1
                              div(class = "enhanced-subject-grade-pair",
                                  div(class = "enhanced-subject-card",
                                      div(class = "enhanced-field-label", "Subject 1 (Required)"),
                                      selectInput("subject1", NULL,
                                                  choices = c("Select Subject" = "", all_subjects),
                                                  width = "100%")
                                  ),
                                  div(class = "enhanced-grade-card",
                                      div(class = "enhanced-field-label", "Grade"),
                                      selectInput("grade1", NULL,
                                                  choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                                  width = "100%")
                                  )
                              ),
                              
                              # Enhanced Subject-Grade Pair 2
                              div(class = "enhanced-subject-grade-pair",
                                  div(class = "enhanced-subject-card",
                                      div(class = "enhanced-field-label", "Subject 2 (Required)"),
                                      selectInput("subject2", NULL,
                                                  choices = c("Select Subject" = "", all_subjects),
                                                  width = "100%")
                                  ),
                                  div(class = "enhanced-grade-card",
                                      div(class = "enhanced-field-label", "Grade"),
                                      selectInput("grade2", NULL,
                                                  choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                                  width = "100%")
                                  )
                              ),
                              
                              # Enhanced Subject-Grade Pair 3
                              div(class = "enhanced-subject-grade-pair",
                                  div(class = "enhanced-subject-card",
                                      div(class = "enhanced-field-label", "Subject 3 (Required)"),
                                      selectInput("subject3", NULL,
                                                  choices = c("Select Subject" = "", all_subjects),
                                                  width = "100%")
                                  ),
                                  div(class = "enhanced-grade-card",
                                      div(class = "enhanced-field-label", "Grade"),
                                      selectInput("grade3", NULL,
                                                  choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                                  width = "100%")
                                  )
                              ),
                              
                              # Enhanced Subject-Grade Pair 4 (Optional)
                              div(class = "enhanced-subject-grade-pair optional-field",
                                  div(class = "enhanced-subject-card",
                                      div(class = "enhanced-field-label", "Subject 4 (Optional)"),
                                      selectInput("subject4", NULL,
                                                  choices = c("Select Subject" = "", all_subjects),
                                                  width = "100%")
                                  ),
                                  div(class = "enhanced-grade-card",
                                      div(class = "enhanced-field-label", "Grade"),
                                      selectInput("grade4", NULL,
                                                  choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                                  width = "100%")
                                  )
                              )
                          )
                   ),
                   
                   # Column 2: Subjects of Interest (Enhanced)
                   column(5, class = "section-column",
                          div(class = "section-title-enhanced", "Subjects of Interest"),
                          div(style = "color: #8db4c9; font-size: 12px; margin-bottom: 20px; text-align: center; font-weight: 500;", 
                              "Choose areas that interest you"),
                          
                          div(class = "enhanced-interest-grid",
                              # All 14 categories as enhanced cards
                              div(class = "enhanced-interest-card",
                                  id = "interest_natural_sciences", onclick = "toggleInterest('natural_sciences', event)", "Natural Sciences"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_humanities", onclick = "toggleInterest('humanities', event)", "Humanities"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_architecture", onclick = "toggleInterest('architecture', event)", "Architecture"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_computational", onclick = "toggleInterest('computational', event)", "Computational & Mathematical Sciences"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_social_sciences", onclick = "toggleInterest('social_sciences', event)", "Social Sciences"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_management", onclick = "toggleInterest('management', event)", "Management"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_medicine", onclick = "toggleInterest('medicine', event)", "Medicine"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_sustainability", onclick = "toggleInterest('sustainability', event)", "Sustainability"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_engineering", onclick = "toggleInterest('engineering', event)", "Engineering"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_languages", onclick = "toggleInterest('languages', event)", "Languages"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_arts", onclick = "toggleInterest('arts', event)", "Arts"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_education", onclick = "toggleInterest('education', event)", "Education"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_technology", onclick = "toggleInterest('technology', event)", "Technology"),
                              div(class = "enhanced-interest-card",
                                  id = "interest_law", onclick = "toggleInterest('law', event)", "Law")
                          )
                   ),
                   
                   # Column 3: Degree Types (Significantly Enhanced)
                   column(3, class = "section-column",
                          div(class = "section-title-enhanced", "Degree Types"),
                          div(style = "color: #8db4c9; font-size: 12px; margin-bottom: 20px; text-align: center; font-weight: 500;", 
                              "Filter by qualification type"),
                          
                          div(class = "enhanced-degree-grid",
                              # Enhanced degree type cards - much bigger and more prominent
                              div(class = "enhanced-degree-card",
                                  id = "interest_ba", onclick = "toggleInterest('ba', event)", "BA"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_bsc", onclick = "toggleInterest('bsc', event)", "BSc"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_msci", onclick = "toggleInterest('msci', event)", "MSci"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_basc", onclick = "toggleInterest('basc', event)", "BASc"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_llb", onclick = "toggleInterest('llb', event)", "LLB"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_beng", onclick = "toggleInterest('beng', event)", "BEng"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_meng", onclick = "toggleInterest('meng', event)", "MEng"),
                              div(class = "enhanced-degree-card small-text",
                                  id = "interest_bscecon", onclick = "toggleInterest('bscecon', event)", "BSc (Econ)"),
                              div(class = "enhanced-degree-card small-text",
                                  id = "interest_mbbsbsc", onclick = "toggleInterest('mbbsbsc', event)", "MBBS BSc"),
                              div(class = "enhanced-degree-card",
                                  id = "interest_mpharm", onclick = "toggleInterest('mpharm', event)", "MPharm")
                          )
                   )
          )
      ),
      
      # Controls section moved below main block
      div(style = "padding: 20px 100px; margin: 10px 80px;",
          div(class = "controls-moved",
              div(style = "color: white; font-weight: bold; margin-bottom: 15px; font-size: 20px; text-align: center;", "Controls"),
              div(style = "display: flex; justify-content: center; gap: 20px;",
                  tags$button("Hide Salary Information", class = "control-btn-wide", style = "width: auto; padding: 10px 25px;"),
                  tags$button("Hide Contextual Grades", class = "control-btn-wide", style = "width: auto; padding: 10px 25px;")
              )
          )
      ),
      
      # Submit Button
      actionButton("submit_filters", "Find My Courses", class = "submit-btn"),
      
      # Filter buttons with proper JavaScript functionality
      div(class = "filter-section",
          div(style = "display: flex; align-items: center; justify-content: space-between; max-width: 1400px; margin: 0 auto; padding: 0 20px;",
              # Left side: Filter buttons
              div(style = "display: flex; gap: 8px;",
                  tags$button("All Suitable Courses", class = "filter-btn active", id = "tab_all", 
                              onclick = "switchTab('all')"),
                  tags$button("Exact Grade Matches", class = "filter-btn", id = "tab_exact", 
                              onclick = "switchTab('exact')"),
                  tags$button("Overqualified Matches", class = "filter-btn", id = "tab_over", 
                              onclick = "switchTab('over')")
              ),
              # Right side: Sorting and number of courses
              div(style = "display: flex; align-items: center; gap: 15px;",
                  div(style = "display: flex; align-items: center; gap: 10px;",
                      span("Sort by:", style = "color: white; font-size: 14px;"),
                      selectInput("sort_by", NULL,
                                  choices = list("Match Quality" = "match", "Grade Requirement" = "grade", "Alphabetical" = "alpha"),
                                  selected = "match",
                                  width = "120px")
                  ),
                  div(style = "display: flex; align-items: center; gap: 10px;",
                      span("Show:", style = "color: white; font-size: 14px;"),
                      selectInput("num_courses", NULL,
                                  choices = list("4" = 4, "16" = 16, "64" = 64, "All" = "all"),
                                  selected = 4,
                                  width = "80px")
                  )
              )
          )
      ),
      
      # Course cards
      div(class = "courses-section",
          uiOutput("course_cards")
      ),
      
      # Recommended section
      div(class = "recommended-section",
          div(class = "section-title", "Recommended for you"),
          div(class = "recommended-grid",
              div(class = "recommended-card",
                  div(class = "recommended-image"),
                  div(class = "recommended-title", "UCAS Degree Tool")
              ),
              div(class = "recommended-card",
                  div(class = "recommended-image"),
                  div(class = "recommended-title", "Tracker finance applications")
              ),
              div(class = "recommended-card",
                  div(class = "recommended-image"),
                  div(class = "recommended-title", "other link")
              ),
              div(class = "recommended-card",
                  div(class = "recommended-image"),
                  div(class = "recommended-title", "Franz Ferdinand, Mitski, Artic Monkeys and...")
              )
          )
      ),
      
      # Add some bottom padding
      div(style = "height: 100px; background-color: #2c2c2c;")
  ),
  
  # Custom Modal
  div(id = "courseModal", class = "custom-modal",
      div(class = "modal-content-custom",
          span(class = "modal-close", "×"),
          
          # Back button
          tags$button("← Go Back", class = "go-back-btn", onclick = "closeModal()"),
          
          # Single row header with course info + grade match + subject requirements
          div(class = "course-detail-header",
              div(class = "header-item", id = "modal_degree_type", "BSc"),
              div(class = "header-title", id = "modal_subject_title", "Subject Title"),
              div(class = "header-item", id = "modal_university_name", "UCL"),
              div(class = "header-item", id = "modal_grade_req", "Grade Req: A"),
              div(style = "flex: 1; display: flex; justify-content: center; align-items: center; gap: 8px;",
                  div(style = "background: #4CAF50; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600;", 
                      id = "modal_match_badge", "Exact Match"),
                  div(style = "background: #2196F3; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600; display: none;", 
                      id = "modal_subject_badge", "Subject Requirements Met")
              )
          ),
          
          # Stats row (updated with offer rate)
          div(class = "stats-row",
              div(class = "stat-card",
                  div(class = "stat-label", "Median Salary"),
                  div(class = "stat-value", id = "modal_salary", "£30,000")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Offer Rate"),
                  div(class = "stat-value", id = "modal_offer_rate", "Data Not Available")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Course Duration"),
                  div(class = "stat-value", id = "modal_duration", "3 Years")
              )
          ),
          
          # Requirements textbox with matched subjects indicator
          div(style = "margin-bottom: 25px;",
              div(style = "width: 100%; background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%); border-radius: 12px; padding: 20px; border: 1px solid #444;",
                  div(style = "color: #9dc5dc; font-size: 14px; font-weight: 600; margin-bottom: 10px; text-transform: uppercase;", "Requirements & Options"),
                  div(style = "color: #ccc; font-size: 14px; line-height: 1.5; margin-bottom: 15px;", id = "modal_requirements", 
                      "A-Level Subjects: Mathematics, Physics or Chemistry required. Year abroad available. Sandwich placement year optional."),
                  # Subject comparison indicator
                  div(style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #555;", id = "matched_subjects_indicator",
                      div(style = "color: #9dc5dc; font-size: 12px; font-weight: 600; margin-bottom: 8px; text-transform: uppercase;", "How Your Subjects Compare"),
                      div(style = "color: #ccc; font-size: 14px; font-weight: bold;", id = "matched_subjects_list", "Select subjects to see comparison")
                  )
              )
          ),
          
          # Two column layout
          div(class = "modal-two-columns",
              # Left: Chart section
              div(class = "chart-section",
                  "bar plot of graduate outcomes by chosen feature"
              ),
              
              # Right: Features and controls
              div(class = "features-section",
                  div(class = "features-title", "Choose Features to Display"),
                  div(class = "features-toggle", "Toggle"),
                  div(class = "features-options", 
                      "(options = Industry/Sector, Salary, Job Title, Location)"),
                  
                  # University website button
                  tags$button("View on University Website", class = "university-btn", 
                              id = "university_website_btn", onclick = ""),
                  
                  # Similar degrees
                  div(class = "similar-degrees",
                      div(class = "features-title", "Similar Degrees"),
                      div(id = "similar_degrees_container",
                          # This will be populated dynamically
                          div(class = "similar-degree-card",
                              div(class = "similar-degree-info",
                                  div(class = "similar-degree-title", "Loading..."),
                                  div(class = "similar-degree-details", "Finding similar courses...")
                              ),
                              tags$button("Course URL", class = "similar-degree-btn"),
                              tags$button("Learn More", class = "similar-degree-btn")
                          )
                      )
                  )
              )
          )
      )
  ),
  
  # JavaScript for modal and toggle functionality
  tags$script(HTML("
    // Modal functions
    function openModal(courseIndex) {
      document.getElementById('courseModal').style.display = 'block';
      Shiny.setInputValue('selected_course_index', courseIndex);
    }
    
    function closeModal() {
      document.getElementById('courseModal').style.display = 'none';
    }
    
    // Close modal when clicking outside
    window.onclick = function(event) {
      var modal = document.getElementById('courseModal');
      if (event.target == modal) {
        closeModal();
      }
    }
    
    // Close modal when clicking X
    document.querySelector('.modal-close').onclick = function() {
      closeModal();
    }
    
    // Tab switching functionality
    function switchTab(tabName) {
      // Update active tab styling
      document.querySelectorAll('.filter-btn').forEach(btn => btn.classList.remove('active'));
      document.getElementById('tab_' + tabName).classList.add('active');
      
      // Send tab change to Shiny
      Shiny.setInputValue('current_tab', tabName);
    }
    
    // Toggle interest subjects - FIXED to prevent click-through
    function toggleInterest(subject, event) {
      if (event) {
        event.stopPropagation();
        event.preventDefault();
      }
      var element = document.getElementById('interest_' + subject);
      if (element) {
        element.classList.toggle('selected');
        Shiny.setInputValue('interest_' + subject, element.classList.contains('selected'));
      }
    }
    
    // Update modal content - FIXED VERSION with matched subjects
    Shiny.addCustomMessageHandler('updateModal', function(data) {
      document.getElementById('modal_degree_type').innerText = data.degree_type;
      document.getElementById('modal_subject_title').innerText = data.title;
      document.getElementById('modal_university_name').innerText = data.university;
      document.getElementById('modal_grade_req').innerText = data.grade_req;
      document.getElementById('modal_salary').innerText = data.salary;
      document.getElementById('modal_offer_rate').innerText = data.offer_rate;

      // Update requirements text with real data
      document.getElementById('modal_requirements').innerText = data.requirements;

      // Update match badge with proper color
      var matchBadge = document.getElementById('modal_match_badge');
      var matchColor = '#666666'; // default
      switch(data.match_type) {
        case 'Exact Match': matchColor = '#4CAF50'; break;
        case 'Good Match': matchColor = '#2196F3'; break;
        case 'Overmatch': matchColor = '#FF9800'; break;
        case 'No Match': matchColor = '#f44336'; break;
      }
      matchBadge.style.backgroundColor = matchColor;
      matchBadge.innerText = data.match_type;

      // *** FIXED: Update subject comparison display ***
      var matchedSubjectsContainer = document.getElementById('matched_subjects_indicator');
      var matchedSubjectsList = document.getElementById('matched_subjects_list');
      
      // Always show the section
      matchedSubjectsContainer.style.display = 'block';
      
      if (!data.has_subject_selection) {
        // User hasn't selected any subjects yet
        matchedSubjectsList.innerText = 'Select subjects to see comparison';
        matchedSubjectsList.style.color = '#ccc'; // Neutral gray
      } else if (data.matched_subjects && data.matched_subjects.length > 0) {
        // User has selected subjects and found matches
        matchedSubjectsList.innerText = data.matched_subjects.join(', ');
        matchedSubjectsList.style.color = '#4CAF50'; // Green for matches
      } else {
        // User has selected subjects but no matches found
        matchedSubjectsList.innerText = 'No matching A-Level subjects found';
        matchedSubjectsList.style.color = '#f44336'; // Red for no matches
      }

      // Update university website button
      document.getElementById('university_website_btn').onclick = function() {
        window.open(data.url, '_blank');
      };

      // Update similar degrees
      var similarContainer = document.getElementById('similar_degrees_container');
      if(data.similar_courses && data.similar_courses.length > 0) {
        var similarHTML = '';
        data.similar_courses.forEach(function(course, index) {
          similarHTML += '<div class=\"similar-degree-card\">' +
            '<div class=\"similar-degree-info\">' +
              '<div class=\"similar-degree-title\">' + course.title + '</div>' +
              '<div class=\"similar-degree-details\">' + course.university + ' ' + course.degree_type + ' Grade Req: ' + course.grade_req + '</div>' +
            '</div>' +
            '<button class=\"similar-degree-btn\" onclick=\"window.open(\\'' + course.url + '\\', \\'_blank\\')\">Course URL</button>' +
            '<button class=\"similar-degree-btn\" onclick=\"openModal(' + (index + 1000) + ')\">Learn More</button>' +
          '</div>';
        });
    
    // Update just the subject comparison section when subjects change
    Shiny.addCustomMessageHandler('updateSubjectComparison', function(data) {
      console.log('Updating subject comparison with:', data);
      
      var matchedSubjectsContainer = document.getElementById('matched_subjects_indicator');
      var matchedSubjectsList = document.getElementById('matched_subjects_list');
      
      if (!matchedSubjectsContainer || !matchedSubjectsList) {
        console.log('Modal elements not found');
        return;
      }
      
      // Always show the section
      matchedSubjectsContainer.style.display = 'block';
      
      if (!data.has_subject_selection) {
        // User hasn't selected any subjects yet
        matchedSubjectsList.innerText = 'Select subjects to see comparison';
        matchedSubjectsList.style.color = '#ccc'; // Neutral gray
        console.log('No subjects selected');
      } else if (data.matched_subjects && data.matched_subjects.length > 0) {
        // User has selected subjects and found matches
        matchedSubjectsList.innerText = data.matched_subjects.join(', ');
        matchedSubjectsList.style.color = '#4CAF50'; // Green for matches
        console.log('Matches found:', data.matched_subjects);
      } else {
        // User has selected subjects but no matches found
        matchedSubjectsList.innerText = 'No matching A-Level subjects found';
        matchedSubjectsList.style.color = '#f44336'; // Red for no matches
        console.log('No matches found');
      }
    });
        similarContainer.innerHTML = similarHTML;
      } else {
        similarContainer.innerHTML = '<div style=\"text-align: center; color: #ccc; padding: 20px;\">No similar courses found</div>';
      }
    });
    
    // Add visual feedback for grade selection
    function updateGradeCardStyle(gradeInputId) {
      const gradeInput = document.getElementById(gradeInputId);
      const gradeCard = gradeInput.closest('.grade-card');
      
      if (gradeInput.value !== '') {
        gradeCard.style.borderColor = '#9dc5dc';
        gradeCard.style.boxShadow = '0 4px 15px rgba(157, 197, 220, 0.2)';
      } else {
        gradeCard.style.borderColor = '#444';
        gradeCard.style.boxShadow = '0 4px 15px rgba(0, 0, 0, 0.2)';
      }
    }
    
    // Monitor grade input changes
    ['grade1', 'grade2', 'grade3', 'grade4'].forEach(function(gradeId) {
      const element = document.getElementById(gradeId);
      if (element) {
        element.addEventListener('change', function() {
          updateGradeCardStyle(gradeId);
        });
      }
    });
  "))
)

# Server
server <- function(input, output, session) {
  
  # Reactive function to get current selected subjects (updates dynamically)
  current_selected_subjects <- reactive({
    get_selected_subjects(input$subject1, input$subject2, input$subject3, input$subject4)
  })
  
  # Main filtering logic (triggered by submit button)
  filtered_courses <- eventReactive(input$submit_filters, {
    data <- ucl_data
    
    # Get selected subjects - LOG WHEN FIND MY COURSES IS CLICKED
    student_subjects <- get_selected_subjects(input$subject1, input$subject2, input$subject3, input$subject4)
    
    # Log selected subjects for debugging (optional)
    if(length(student_subjects) > 0) {
      cat("Selected subjects:", paste(student_subjects, collapse = ", "), "\n")
    }
    
    # Calculate student's score from their grades - FIXED DEFAULT HANDLING
    student_grades <- c(input$grade1, input$grade2, input$grade3, input$grade4)
    student_grades <- student_grades[!is.null(student_grades) & student_grades != ""]
    
    if(length(student_grades) >= 3) {
      student_score <- calculate_student_score(student_grades)
      
      # Add match types to data
      data$match_type <- sapply(data$grade_score, function(course_score) {
        get_match_type(student_score, course_score)
      })
      
      # Sort by match type priority (Exact, Good, Overmatch, No Match, No Data)
      match_order <- c("Exact Match", "Good Match", "Overmatch", "No Match", "No Data")
      data$match_priority <- match(data$match_type, match_order)
      data <- data[order(data$match_priority, -data$grade_score, na.last = TRUE), ]
    } else {
      # Default: no grade matching, just show all courses sorted by grade requirement
      data$match_type <- "No Data"
      data$match_priority <- 5
      student_score <- NA
    }
    
    # Add subject requirements matching
    if(length(student_subjects) > 0) {
      data$subject_requirements_met <- sapply(1:nrow(data), function(i) {
        course_requirements <- if(!is.null(data$a_level_subjects[i]) && !is.na(data$a_level_subjects[i])) {
          data$a_level_subjects[i]
        } else {
          ""
        }
        match_subjects_with_requirements(student_subjects, course_requirements)
      })
      
      # Store selected subjects for later use in modal
      data$selected_subjects <- list(student_subjects)
    } else {
      data$subject_requirements_met <- FALSE
      data$selected_subjects <- list(character(0))
    }
    
    # Store student score for later use
    data$student_score <- student_score
    
    # ALL SELECTIONS USE THE SAME "interest_" SYSTEM
    
    # 1. Filter by degree types (using correct degree type names)
    selected_degrees <- c()
    if(!is.null(input$interest_ba) && input$interest_ba) selected_degrees <- c(selected_degrees, "BA")
    if(!is.null(input$interest_bsc) && input$interest_bsc) selected_degrees <- c(selected_degrees, "BSc")
    if(!is.null(input$interest_msci) && input$interest_msci) selected_degrees <- c(selected_degrees, "MSCi")
    if(!is.null(input$interest_basc) && input$interest_basc) selected_degrees <- c(selected_degrees, "BASc")
    if(!is.null(input$interest_llb) && input$interest_llb) selected_degrees <- c(selected_degrees, "LLB")
    if(!is.null(input$interest_beng) && input$interest_beng) selected_degrees <- c(selected_degrees, "BEng")
    if(!is.null(input$interest_meng) && input$interest_meng) selected_degrees <- c(selected_degrees, "MEng")
    if(!is.null(input$interest_bscecon) && input$interest_bscecon) selected_degrees <- c(selected_degrees, "BSc (Econ)")
    if(!is.null(input$interest_mbbsbsc) && input$interest_mbbsbsc) selected_degrees <- c(selected_degrees, "MBBS BSc")
    if(!is.null(input$interest_mpharm) && input$interest_mpharm) selected_degrees <- c(selected_degrees, "MPharm")
    
    # 2. Filter by subject interests (ALL 14 CATEGORIES) - CHANGED TO "OR" LOGIC
    selected_subjects <- c()
    if(!is.null(input$interest_natural_sciences) && input$interest_natural_sciences) {
      selected_subjects <- c(selected_subjects, "Natural Sciences")
    }
    if(!is.null(input$interest_humanities) && input$interest_humanities) {
      selected_subjects <- c(selected_subjects, "Humanities")
    }
    if(!is.null(input$interest_architecture) && input$interest_architecture) {
      selected_subjects <- c(selected_subjects, "Architecture")
    }
    if(!is.null(input$interest_computational) && input$interest_computational) {
      selected_subjects <- c(selected_subjects, "Computational & Mathematical Sciences")
    }
    if(!is.null(input$interest_social_sciences) && input$interest_social_sciences) {
      selected_subjects <- c(selected_subjects, "Social Sciences")
    }
    if(!is.null(input$interest_management) && input$interest_management) {
      selected_subjects <- c(selected_subjects, "Management")
    }
    if(!is.null(input$interest_medicine) && input$interest_medicine) {
      selected_subjects <- c(selected_subjects, "Medicine")
    }
    if(!is.null(input$interest_sustainability) && input$interest_sustainability) {
      selected_subjects <- c(selected_subjects, "Sustainability")
    }
    if(!is.null(input$interest_engineering) && input$interest_engineering) {
      selected_subjects <- c(selected_subjects, "Engineering")
    }
    if(!is.null(input$interest_languages) && input$interest_languages) {
      selected_subjects <- c(selected_subjects, "Languages")
    }
    if(!is.null(input$interest_arts) && input$interest_arts) {
      selected_subjects <- c(selected_subjects, "Arts")
    }
    if(!is.null(input$interest_education) && input$interest_education) {
      selected_subjects <- c(selected_subjects, "Education")
    }
    if(!is.null(input$interest_technology) && input$interest_technology) {
      selected_subjects <- c(selected_subjects, "Technology")
    }
    if(!is.null(input$interest_law) && input$interest_law) {
      selected_subjects <- c(selected_subjects, "Law")
    }
    
    # Apply subject filter using OR logic (if any subjects are selected)
    if(length(selected_subjects) > 0) {
      # Collect all courses that match ANY of the selected subjects
      all_matching_courses <- data.frame()
      
      for(subject in selected_subjects) {
        subject_courses <- get_subject_category_courses(subject, ucl_data)  # Use original dataset
        all_matching_courses <- rbind(all_matching_courses, subject_courses)
      }
      
      # Remove duplicates (courses that appear in multiple categories)
      subject_filtered_data <- all_matching_courses[!duplicated(all_matching_courses$title), ]
      
      # Keep only courses that exist in both the subject filter AND our working dataset
      data <- data[data$title %in% subject_filtered_data$title, ]
    }
    
    # 3. Apply degree filter (only if degrees are selected)
    if(length(selected_degrees) > 0) {
      data <- data[data$degree_type %in% selected_degrees, ]
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # Store the currently displayed courses for modal access
  displayed_courses <- reactive({
    # Use filtered data if submit has been pressed, otherwise show all courses
    if(input$submit_filters == 0) {
      filtered_data <- ucl_data
      filtered_data$match_type <- "No Data"
      filtered_data$subject_requirements_met <- FALSE
      filtered_data$selected_subjects <- list(character(0))
    } else {
      filtered_data <- filtered_courses()
    }
    
    # Apply tab filtering based on current active tab
    current_tab <- if(is.null(input$current_tab)) "all" else input$current_tab
    
    if(current_tab == "exact" && any(!is.na(filtered_data$match_type))) {
      filtered_data <- filtered_data[filtered_data$match_type == "Exact Match", ]
    } else if(current_tab == "over" && any(!is.na(filtered_data$match_type))) {
      filtered_data <- filtered_data[filtered_data$match_type %in% c("Good Match", "Overmatch"), ]
    } else if(current_tab == "all" && any(!is.na(filtered_data$match_type))) {
      # Show only courses they qualify for (exclude "No Match")
      filtered_data <- filtered_data[filtered_data$match_type %in% c("Exact Match", "Good Match", "Overmatch", "No Data"), ]
    }
    
    # Apply sorting based on user selection
    if(!is.null(input$sort_by)) {
      if(input$sort_by == "alpha") {
        filtered_data <- filtered_data[order(filtered_data$title), ]
      } else if(input$sort_by == "grade") {
        filtered_data <- filtered_data[order(-filtered_data$grade_score, na.last = TRUE), ]
      } else if(input$sort_by == "match") {
        # Default match quality sorting (already applied in filtered_courses)
        if(any(!is.na(filtered_data$match_priority))) {
          filtered_data <- filtered_data[order(filtered_data$match_priority, -filtered_data$grade_score, na.last = TRUE), ]
        }
      }
    }
    
    # Determine number of courses to show based on user selection
    num_to_show <- if(input$num_courses == "all") {
      nrow(filtered_data)
    } else {
      min(as.numeric(input$num_courses), nrow(filtered_data))
    }
    
    if(nrow(filtered_data) == 0) {
      return(data.frame()) # Return empty dataframe
    }
    
    # Return the courses that will be displayed
    return(head(filtered_data, num_to_show))
  })
  output$course_cards <- renderUI({
    # Get the courses that should be displayed
    courses_to_show <- displayed_courses()
    
    if(nrow(courses_to_show) == 0) {
      return(div(style = "text-align: center; color: white; padding: 50px;",
                 h3("No courses match your criteria"),
                 p("Try adjusting your filters or selections")))
    }
    
    course_cards <- lapply(1:nrow(courses_to_show), function(i) {
      course <- courses_to_show[i, ]
      
      # Determine match type styling
      match_color <- switch(course$match_type,
                            "Exact Match" = "#4CAF50",      # Green
                            "Good Match" = "#2196F3",       # Blue  
                            "Overmatch" = "#FF9800",        # Orange
                            "No Match" = "#f44336",         # Red
                            "#666666"                       # Default gray
      )
      
      # Use actual university name from data instead of hardcoded "UCL"
      university_name <- if(!is.null(course$university) && !is.na(course$university) && course$university != "") {
        course$university
      } else {
        "University" # fallback if university data is missing
      }
      
      div(class = "course-card",
          # Add match type indicator (only if grades have been entered)
          if(!is.null(course$match_type) && course$match_type != "No Data") {
            div(style = paste0("background-color: ", match_color, "; color: white; padding: 4px 8px; border-radius: 10px; font-size: 12px; margin-bottom: 8px; display: inline-block;"),
                course$match_type)
          },
          div(class = "course-title", 
              onclick = paste0("openModal(", i, ")"),
              course$title),
          div(class = "course-details", 
              paste(university_name, "•", course$degree_type, "• Grade Req:", course$a_level)),
          div(
            tags$button("Course URL", class = "course-btn", 
                        onclick = paste0("window.open('", course$url, "', '_blank')")),
            tags$button("Learn More", class = "course-btn",
                        onclick = paste0("openModal(", i, ")"))
          )
      )
    })
    
    # Arrange in proper 2-column grid
    div(class = "courses-grid", course_cards)
  })
  
  # Prevent duplicate subject selection across dropdowns - FIXED APPROACH
  observeEvent(c(input$subject1, input$subject2, input$subject3), {
    # Get currently selected subjects
    selected_subjects <- c(input$subject1, input$subject2, input$subject3)
    selected_subjects <- selected_subjects[!is.null(selected_subjects) & selected_subjects != ""]
    
    # Only update subject4 choices to exclude already selected subjects
    available_for_subject4 <- all_subjects[!all_subjects %in% selected_subjects]
    updateSelectInput(session, "subject4", 
                      choices = c("Select Subject" = "", available_for_subject4))
  }, ignoreInit = TRUE)
  
  # Validation: Enable Grade/Subject 4 when first 3 are filled
  observe({
    subjects_123_filled <- !is.null(input$subject1) && input$subject1 != "" &&
      !is.null(input$subject2) && input$subject2 != "" &&
      !is.null(input$subject3) && input$subject3 != ""
    
    grades_123_filled <- !is.null(input$grade1) && input$grade1 != "" &&
      !is.null(input$grade2) && input$grade2 != "" &&
      !is.null(input$grade3) && input$grade3 != ""
    
    if(!subjects_123_filled || !grades_123_filled) {
      updateSelectInput(session, "grade4", choices = c("Complete first 3 subjects & grades" = ""))
    } else {
      updateSelectInput(session, "grade4", choices = c("Select Grade" = "", "A*", "A", "B", "C", "D", "E"))
    }
  })
  
  observeEvent(input$selected_course_index, {
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      # Use the same dataset that was used to display the course cards
      current_data <- displayed_courses()
      
      # Make sure the selected index is within range
      if(input$selected_course_index <= nrow(current_data)) {
        # Get course from the displayed dataset
        course <- current_data[input$selected_course_index, ]
        
        # Format salary with proper handling of NA values
        salary_text <- if(!is.na(course$median_salary) && course$median_salary > 0) {
          paste("£", format(course$median_salary, big.mark = ",", scientific = FALSE))
        } else {
          "N/A"
        }
        
        # Format offer rate with percentage sign
        offer_rate_text <- if(!is.null(course$offer_rate_clean) && !is.na(course$offer_rate_clean) && course$offer_rate_clean != "") {
          if(course$offer_rate_clean == "Data Not Available") {
            "Data Not Available"
          } else {
            # Add percentage sign to numeric values
            paste0(course$offer_rate_clean, "%")
          }
        } else {
          "Data Not Available"
        }
        
        # Get university name
        university_name <- if(!is.null(course$university) && !is.na(course$university) && course$university != "") {
          course$university
        } else {
          "University"
        }
        
        # Get match type and subject requirements
        match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        subject_requirements_met <- if(!is.null(course$subject_requirements_met)) course$subject_requirements_met else FALSE
        
        # Get selected subjects for highlighting - USE CURRENT REACTIVE SUBJECTS
        selected_subjects <- current_selected_subjects()
        
        # Check if user has selected any subjects
        has_subject_selection <- length(selected_subjects) > 0
        
        # Build requirements text from real data - CLEAN, NO HTML + DEBUG
        requirements_text <- ""
        
        # Add A-level subjects (main requirement)
        if(!is.null(course$a_level_subjects) && !is.na(course$a_level_subjects) && course$a_level_subjects != "") {
          requirements_text <- course$a_level_subjects
          cat("Found a_level_subjects:", course$a_level_subjects, "\n")
        } else {
          cat("No a_level_subjects found for course:", course$title, "\n")
        }
        
        # Add placement year info if available
        placement_text <- ""
        if(!is.null(course$sandwich) && !is.na(course$sandwich)) {
          if(course$sandwich == "Yes") {
            placement_text <- "Placement year available."
          }
        }
        
        # Add year abroad info if available  
        abroad_text <- ""
        if(!is.null(course$yearabroad) && !is.na(course$yearabroad)) {
          if(course$yearabroad == "Yes") {
            abroad_text <- "Year abroad available."
          }
        }
        
        # Combine all requirements text
        additional_options <- c(placement_text, abroad_text)
        additional_options <- additional_options[additional_options != ""]
        
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        
        # Fallback if no requirements data
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        
        cat("Final requirements text:", requirements_text, "\n")
        
        # Find matched subjects using YOUR working logic
        matched_subjects_list <- if(has_subject_selection && requirements_text != "") {
          find_matched_subjects(selected_subjects, requirements_text)
        } else {
          character(0)
        }
        
        # Find similar courses from the full dataset (not just displayed courses)
        full_dataset <- if(input$submit_filters == 0) ucl_data else filtered_courses()
        similar_courses <- find_similar_courses(course, full_dataset, limit = 3)
        
        # Update modal content with CLEAN requirements and SIMPLE matching
        session$sendCustomMessage("updateModal", list(
          degree_type = course$degree_type,
          title = course$title,
          university = university_name,
          grade_req = paste("Grade Req:", course$a_level),
          salary = salary_text,
          offer_rate = offer_rate_text,
          url = course$url,
          match_type = match_type,
          subject_requirements_met = subject_requirements_met,
          has_subject_selection = has_subject_selection,
          requirements = requirements_text,  # CLEAN TEXT
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL,
          similar_courses = if(nrow(similar_courses) > 0) {
            lapply(1:nrow(similar_courses), function(i) {
              sim_course <- similar_courses[i, ]
              sim_university <- if(!is.null(sim_course$university) && !is.na(sim_course$university) && sim_course$university != "") {
                sim_course$university
              } else {
                "University"
              }
              list(
                title = sim_course$title,
                degree_type = sim_course$degree_type,
                university = sim_university,
                grade_req = sim_course$a_level,
                url = sim_course$url
              )
            })
          } else {
            list()
          }
        ))
        
      }
    }
  })
  
  # Update modal when subjects change (if modal is open)
  observeEvent(current_selected_subjects(), {
    cat("Subjects changed to:", paste(current_selected_subjects(), collapse = ", "), "\n")
    
    # Check if a modal is currently open and re-trigger the modal update
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      cat("Modal is open, updating subject comparison\n")
      
      # Get current modal course data
      current_data <- displayed_courses()
      if(input$selected_course_index <= nrow(current_data)) {
        # Re-trigger the modal update with new subject matching
        course <- current_data[input$selected_course_index, ]
        
        # Get current selected subjects
        selected_subjects <- current_selected_subjects()
        has_subject_selection <- length(selected_subjects) > 0
        
        cat("Current subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Build requirements text from real data
        requirements_text <- ""
        
        # Add A-level subjects (main requirement)
        if(!is.null(course$a_level_subjects) && !is.na(course$a_level_subjects) && course$a_level_subjects != "") {
          requirements_text <- course$a_level_subjects
        }
        
        # Add placement year info if available
        placement_text <- ""
        if(!is.null(course$sandwich) && !is.na(course$sandwich)) {
          if(course$sandwich == "Yes") {
            placement_text <- "Placement year available."
          }
        }
        
        # Add year abroad info if available  
        abroad_text <- ""
        if(!is.null(course$yearabroad) && !is.na(course$yearabroad)) {
          if(course$yearabroad == "Yes") {
            abroad_text <- "Year abroad available."
          }
        }
        
        # Combine all requirements text
        additional_options <- c(placement_text, abroad_text)
        additional_options <- additional_options[additional_options != ""]
        
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        
        # Fallback if no requirements data
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        
        cat("Requirements text:", requirements_text, "\n")
        
        # Find matched subjects using YOUR working logic
        matched_subjects_list <- if(has_subject_selection && requirements_text != "") {
          find_matched_subjects(selected_subjects, requirements_text)
        } else {
          character(0)
        }
        
        cat("Matched subjects:", paste(matched_subjects_list, collapse = ", "), "\n")
        
        # Update only the subject comparison part of the modal
        session$sendCustomMessage("updateSubjectComparison", list(
          has_subject_selection = has_subject_selection,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL
        ))
      }
    }
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server, options = list(height = 1080))