library(shiny)

# Load the full UCL dataset from GitHub
ucl_data <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/ucl_pcm_degree_v3.csv", 
                     stringsAsFactors = FALSE)

# Clean and prepare the data
ucl_data$median_salary <- as.numeric(ucl_data$median_salary)
ucl_data$lower_quartile_salary <- as.numeric(ucl_data$lower_quartile_salary)
ucl_data$upper_quartile_salary <- as.numeric(ucl_data$upper_quartile_salary)

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

# Function to find similar courses based on subject categories
find_similar_courses <- function(current_course, all_courses, limit = 3) {
  # Get the subject category of the current course
  current_categories <- c()
  
  # Check which categories the current course belongs to
  categories <- c("Natural Sciences", "Humanities", "Architecture", "Computational & Mathematical Sciences",
                  "Social Sciences", "Management", "Medicine", "Sustainability", "Engineering", 
                  "Languages", "Arts", "Education", "Technology", "Law")
  
  for(category in categories) {
    category_courses <- get_subject_category_courses(category, all_courses)
    if(current_course$title %in% category_courses$title) {
      current_categories <- c(current_categories, category)
    }
  }
  
  # Find courses in the same categories
  similar_courses <- data.frame()
  
  for(category in current_categories) {
    category_courses <- get_subject_category_courses(category, all_courses)
    # Remove the current course itself
    category_courses <- category_courses[category_courses$title != current_course$title, ]
    similar_courses <- rbind(similar_courses, category_courses)
  }
  
  # Remove duplicates and limit results
  similar_courses <- similar_courses[!duplicated(similar_courses$title), ]
  
  # Sort by grade requirement (closest to current course first)
  if(nrow(similar_courses) > 0) {
    current_score <- current_course$grade_score
    if(!is.na(current_score)) {
      similar_courses$score_diff <- abs(similar_courses$grade_score - current_score)
      similar_courses <- similar_courses[order(similar_courses$score_diff), ]
    }
    
    return(head(similar_courses, limit))
  }
  
  return(data.frame()) # Return empty if no similar courses found
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

# Core subjects for dropdown (simplified)
core_subjects <- c("Mathematics", "Physics", "Chemistry", "Biology", 
                   "English Literature", "History", "Geography")

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
        font-size: 28px;
        font-weight: bold;
        margin-bottom: 20px;
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
      
      /* UPDATED: Subject Interest Card Styles */
      .subject-interest-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        border-radius: 8px;
        padding: 6px;
        text-align: center;
        font-size: 10px;
        min-height: 20px;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        border: 1px solid #444;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
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
      
      /* UPDATED: Degree Type Card Styles */
      .degree-type-card {
        background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%);
        color: white;
        border-radius: 6px;
        padding: 2px;
        text-align: center;
        font-size: 9px;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        border: 1px solid #444;
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
    "))
  ),
  
  # Header
  div(class = "main-header", "Course Grade Tracker"),
  
  # Main content wrapper
  div(class = "content-wrapper",
      # Top section with grades, interests, and degree types - 3 COLUMN STRUCTURE
      div(style = "padding: 50px 100px; background-color: #3a3a3a; margin: 30px 80px; border-radius: 20px;",
          fluidRow(
            # Column 1: Your Grades (IMPROVED FORMATTING)
            column(4,
                   div(class = "section-title", "Your Grades"),
                   div(class = "grade-section-vertical",
                       # Grade pair 1
                       div(class = "grade-pair",
                           div(class = "grade-card-left", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("subject1", NULL, 
                                           choices = c("Select Subject" = "", core_subjects),
                                           width = "100%")
                           ),
                           div(class = "grade-card-right", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("grade1", NULL,
                                           choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                           width = "100%")
                           )
                       ),
                       # Grade pair 2  
                       div(class = "grade-pair",
                           div(class = "grade-card-left", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("subject2", NULL,
                                           choices = c("Select Subject" = "", core_subjects),
                                           width = "100%")
                           ),
                           div(class = "grade-card-right", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("grade2", NULL,
                                           choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                           width = "100%")
                           )
                       ),
                       # Grade pair 3
                       div(class = "grade-pair",
                           div(class = "grade-card-left", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("subject3", NULL,
                                           choices = c("Select Subject" = "", core_subjects),
                                           width = "100%")
                           ),
                           div(class = "grade-card-right", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("grade3", NULL,
                                           choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                           width = "100%")
                           )
                       ),
                       # Grade pair 4 (Optional - only if first 3 are filled)
                       div(class = "grade-pair",
                           div(class = "grade-card-left", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("subject4", NULL,
                                           choices = c("Select Subject (Optional)" = "", core_subjects),
                                           width = "100%")
                           ),
                           div(class = "grade-card-right", style = "border-radius: 20px; padding: 8px 12px;",
                               selectInput("grade4", NULL,
                                           choices = c("Grade" = "", "A*", "A", "B", "C", "D", "E"),
                                           width = "100%")
                           )
                       )
                   )
            ),
            
            # Column 2: Subjects of Interest (BETTER FORMATTED GRID) - FIXED WITH PROPER CLASSES
            column(5,
                   div(class = "section-title", style = "font-size: 24px;", "Subjects of Interest"),
                   div(style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px; max-width: 450px;",
                       # All 14 categories as cards with full labels
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_natural_sciences", onclick = "toggleInterest('natural_sciences')", "Natural Sciences"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_humanities", onclick = "toggleInterest('humanities')", "Humanities"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_architecture", onclick = "toggleInterest('architecture')", "Architecture"),
                       div(class = "subject-interest-card", style = "font-size: 11px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center; text-align: center;",
                           id = "interest_computational", onclick = "toggleInterest('computational')", "Computational & Mathematical Sciences"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_social_sciences", onclick = "toggleInterest('social_sciences')", "Social Sciences"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_management", onclick = "toggleInterest('management')", "Management"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_medicine", onclick = "toggleInterest('medicine')", "Medicine"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_sustainability", onclick = "toggleInterest('sustainability')", "Sustainability"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_engineering", onclick = "toggleInterest('engineering')", "Engineering"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_languages", onclick = "toggleInterest('languages')", "Languages"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_arts", onclick = "toggleInterest('arts')", "Arts"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_education", onclick = "toggleInterest('education')", "Education"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_technology", onclick = "toggleInterest('technology')", "Technology"),
                       div(class = "subject-interest-card", style = "font-size: 12px; padding: 8px; min-height: 35px; display: flex; align-items: center; justify-content: center;",
                           id = "interest_law", onclick = "toggleInterest('law')", "Law")
                   )
            ),
            
            # Column 3: Degree Types (IMPROVED FORMATTING) - FIXED WITH PROPER CLASSES
            column(3,
                   div(class = "section-title", style = "text-align: right; margin-bottom: 15px; font-size: 24px;", "Degree Types"),
                   
                   div(style = "display: flex; flex-direction: column; align-items: flex-end;",
                       # Degree Types as buttons in 3x2 grid with updated degrees
                       div(style = "display: grid; grid-template-columns: repeat(3, 55px); grid-template-rows: repeat(2, 30px); gap: 6px;",
                           div(class = "degree-type-card", style = "font-size: 11px; padding: 4px;",
                               id = "interest_bsc", onclick = "toggleInterest('bsc')", "BSc"),
                           div(class = "degree-type-card", style = "font-size: 11px; padding: 4px;",
                               id = "interest_msci", onclick = "toggleInterest('msci')", "MSci"),
                           div(class = "degree-type-card", style = "font-size: 11px; padding: 4px;",
                               id = "interest_ba", onclick = "toggleInterest('ba')", "BA"),
                           div(class = "degree-type-card", style = "font-size: 11px; padding: 4px;",
                               id = "interest_meng", onclick = "toggleInterest('meng')", "MEng"),
                           div(class = "degree-type-card", style = "font-size: 11px; padding: 4px;",
                               id = "interest_beng", onclick = "toggleInterest('beng')", "BEng"),
                           div(class = "degree-type-card", style = "font-size: 11px; padding: 4px;",
                               id = "interest_llb", onclick = "toggleInterest('llb')", "LLB")
                       )
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
          
          # Single row header with course info + grade match
          div(class = "course-detail-header",
              div(class = "header-item", id = "modal_degree_type", "BSc"),
              div(class = "header-title", id = "modal_subject_title", "Subject Title"),
              div(class = "header-item", "UCL"),
              div(class = "header-item", id = "modal_grade_req", "Grade Req: A"),
              div(style = "flex: 1; display: flex; justify-content: center; align-items: center;",
                  div(style = "background: #4CAF50; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600;", 
                      id = "modal_match_badge", "Exact Match")
              )
          ),
          
          # Stats row
          div(class = "stats-row",
              div(class = "stat-card",
                  div(class = "stat-label", "Median Salary"),
                  div(class = "stat-value", id = "modal_salary", "£30,000")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Acceptance Rate"),
                  div(class = "stat-value", "TBD")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Course Duration"),
                  div(class = "stat-value", id = "modal_duration", "3 Years")
              )
          ),
          
          # NEW: Requirements textbox (full width, above features)
          div(style = "margin-bottom: 25px;",
              div(style = "width: 100%; background: linear-gradient(135deg, #2d2d2d 0%, #3a3a3a 100%); border-radius: 12px; padding: 20px; border: 1px solid #444;",
                  div(style = "color: #9dc5dc; font-size: 14px; font-weight: 600; margin-bottom: 10px; text-transform: uppercase;", "Requirements & Options"),
                  div(style = "color: #ccc; font-size: 14px; line-height: 1.5;", id = "modal_requirements", 
                      "A-Level Subjects: Mathematics, Physics or Chemistry required. Year abroad available. Sandwich placement year optional.")
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
    
    // Toggle interest subjects
    function toggleInterest(subject) {
      var element = document.getElementById('interest_' + subject);
      element.classList.toggle('selected');
      Shiny.setInputValue('interest_' + subject, element.classList.contains('selected'));
    }
    
    // Update modal content
    Shiny.addCustomMessageHandler('updateModal', function(data) {
      document.getElementById('modal_degree_type').innerText = data.degree_type;
      document.getElementById('modal_subject_title').innerText = data.title;
      document.getElementById('modal_grade_req').innerText = data.grade_req;
      document.getElementById('modal_salary').innerText = data.salary;
      
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
              '<div class=\"similar-degree-details\">UCL ' + course.degree_type + ' Grade Req: ' + course.grade_req + '</div>' +
            '</div>' +
            '<button class=\"similar-degree-btn\" onclick=\"window.open(\\'' + course.url + '\\', \\'_blank\\')\">Course URL</button>' +
            '<button class=\"similar-degree-btn\" onclick=\"openModal(' + (index + 1000) + ')\">Learn More</button>' +
          '</div>';
        });
        similarContainer.innerHTML = similarHTML;
      } else {
        similarContainer.innerHTML = '<div style=\"text-align: center; color: #ccc; padding: 20px;\">No similar courses found</div>';
      }
    });
  "))
)

# Server
server <- function(input, output, session) {
  
  # Main filtering logic (triggered by submit button)
  filtered_courses <- eventReactive(input$submit_filters, {
    data <- ucl_data
    
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
    
    # Store student score for later use
    data$student_score <- student_score
    
    # ALL SELECTIONS USE THE SAME "interest_" SYSTEM
    
    # 1. Filter by degree types (using interest_ system) - EXACT DATASET MATCHING
    selected_degrees <- c()
    if(!is.null(input$interest_bsc) && input$interest_bsc) selected_degrees <- c(selected_degrees, "BSc")
    if(!is.null(input$interest_msci) && input$interest_msci) selected_degrees <- c(selected_degrees, "MSCi")
    if(!is.null(input$interest_ba) && input$interest_ba) selected_degrees <- c(selected_degrees, "BA")
    if(!is.null(input$interest_meng) && input$interest_meng) selected_degrees <- c(selected_degrees, "MEng")
    if(!is.null(input$interest_beng) && input$interest_beng) selected_degrees <- c(selected_degrees, "BEng")
    if(!is.null(input$interest_llb) && input$interest_llb) selected_degrees <- c(selected_degrees, "LLB")
    
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
  
  # Debug: Add a reactive to show what's happening
  output$debug_info <- renderText({
    if(input$submit_filters > 0) {
      student_grades <- c(input$grade1, input$grade2, input$grade3, input$grade4)
      student_grades <- student_grades[!is.null(student_grades) & student_grades != ""]
      
      if(length(student_grades) >= 3) {
        student_score <- calculate_student_score(student_grades)
        
        # Find some example courses
        sample_courses <- head(ucl_data, 10)
        examples <- sapply(1:nrow(sample_courses), function(i) {
          course <- sample_courses[i, ]
          match_type <- get_match_type(student_score, course$grade_score)
          paste0(course$a_level, " (", course$grade_score, ") -> ", match_type)
        })
        
        paste0("Student: ", paste(student_grades, collapse=""), " (", student_score, ")\n",
               "Examples:\n", paste(examples, collapse="\n"))
      }
    }
  })
  output$course_cards <- renderUI({
    # Use filtered data if submit has been pressed, otherwise show all courses
    if(input$submit_filters == 0) {
      filtered_data <- ucl_data
      filtered_data$match_type <- "No Data"
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
      return(div(style = "text-align: center; color: white; padding: 50px;",
                 h3("No courses match your criteria"),
                 p("Try adjusting your filters or selections")))
    }
    
    # Get the courses to display
    courses_to_show <- head(filtered_data, num_to_show)
    
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
              paste("UCL •", course$degree_type, "• Grade Req:", course$a_level)),
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
  
  # Validation: Disable Grade 4 unless Grades 1-3 are filled
  observe({
    grades_123_filled <- !is.null(input$grade1) && input$grade1 != "" &&
      !is.null(input$grade2) && input$grade2 != "" &&
      !is.null(input$grade3) && input$grade3 != ""
    
    if(!grades_123_filled) {
      updateSelectInput(session, "grade4", choices = c("Fill Grades 1-3 first" = ""))
      updateSelectInput(session, "subject4", choices = c("Fill Grades 1-3 first" = ""))
    } else {
      updateSelectInput(session, "grade4", choices = c("Grade (Optional)" = "", "A*", "A", "B", "C", "D", "E"))
      updateSelectInput(session, "subject4", choices = c("Select Subject (Optional)" = "", core_subjects))
    }
  })
  observe({
    grades_123_filled <- !is.null(input$grade1) && input$grade1 != "" &&
      !is.null(input$grade2) && input$grade2 != "" &&
      !is.null(input$grade3) && input$grade3 != ""
    
    if(!grades_123_filled) {
      updateSelectInput(session, "grade4", choices = c("Fill Grades 1-3 first" = ""))
      updateSelectInput(session, "subject4", choices = c("Fill Grades 1-3 first" = ""))
    } else {
      updateSelectInput(session, "grade4", choices = c("Grade (Optional)" = "", "A*", "A", "B", "C", "D", "E"))
      updateSelectInput(session, "subject4", choices = c("Select Subject (Optional)" = "", core_subjects))
    }
  })
  observeEvent(input$selected_course_index, {
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      # Determine which dataset to use
      if(input$submit_filters == 0) {
        current_data <- ucl_data
      } else {
        current_data <- filtered_courses()
      }
      
      # Make sure the selected index is within range
      if(input$selected_course_index <= nrow(current_data)) {
        # Get course from the current dataset
        course <- current_data[input$selected_course_index, ]
        
        # Format salary with proper handling of NA values
        salary_text <- if(!is.na(course$median_salary) && course$median_salary > 0) {
          paste("£", format(course$median_salary, big.mark = ",", scientific = FALSE))
        } else {
          "N/A"
        }
        
        # Get match type
        match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        
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
        
        # Find similar courses
        similar_courses <- find_similar_courses(course, current_data, limit = 3)
        
        # Update modal content
        session$sendCustomMessage("updateModal", list(
          degree_type = course$degree_type,
          title = course$title,
          grade_req = paste("Grade Req:", course$a_level),
          salary = salary_text,
          url = course$url,
          match_type = match_type,
          requirements = requirements_text,
          similar_courses = if(nrow(similar_courses) > 0) {
            lapply(1:nrow(similar_courses), function(i) {
              sim_course <- similar_courses[i, ]
              list(
                title = sim_course$title,
                degree_type = sim_course$degree_type,
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
}

# Run the application
shinyApp(ui = ui, server = server, options = list(height = 1080))