
# A lot of this code given on Campuswire

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the submitbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      rating_matrix <- read.csv("Rmat.csv")
      rating_matrix <- as.matrix(rating_matrix)
      
      # Normalize the rating matrix
      normal = function(rating_matrix){
        rowM = rowMeans(rating_matrix, na.rm = TRUE)
        R_normal <- rating_matrix
        for(i in 1:nrow(rating_matrix)){
          R_normal[i, ] = ifelse(!is.na(rating_matrix[i, ]), rating_matrix[i, ] - rowM[i], NA)
        }
        return(R_normal)
      }
      
      rating_norm = normal(rating_matrix)
      
      #function to calculate cosine similarity
      cos_sim <- function(rating_norm)
      {
        n_movies <- ncol(rating_norm)
        S_matrix <- matrix(NA, nrow = n_movies, ncol = n_movies)
        rownames(S_matrix) <- colnames(S_matrix) <- colnames(rating_matrix)
        
        for (i in 1:(n_movies-1)) {
          for (j in (i+1):n_movies) {
            common_users <- which(!is.na(rating_norm[, i]) & !is.na(rating_norm[, j]))
            if (length(common_users) > 2) {
              Ri <- rating_norm[common_users, i]
              Rj <- rating_norm[common_users, j]
              
              numerator <- sum(Ri * Rj)
              denominator_i <- sqrt(sum(Ri*Ri))
              denominator_j <- sqrt(sum(Rj*Rj))
              
              if (denominator_i > 0 && denominator_j > 0) {
                cosine_sim <- numerator / (denominator_i * denominator_j)
                S_matrix[i, j] <- S_matrix[j, i] <- 0.5 + 0.5 * cosine_sim
              }
            }
          }
        }
        return(S_matrix)
      }
      
      
      # define S matrix
      S <- cos_sim(rating_norm)
      diag(S) <- NA
      rownames(S) <- colnames(rating_matrix)
      colnames(S) <- colnames(rating_matrix)
      
      #  For each row, sort the non-NA similarity measures and keep top 30, rest NA
      S_top_30 <- function(S) {
        n_movies <- nrow(S)
        S_sorted <- matrix(NA, nrow = n_movies, ncol = n_movies)
        rownames(S_sorted) <- rownames(S)
        colnames(S_sorted) <- colnames(S)
        for (i in 1:n_movies){
          row <- as.numeric(S[i, ])
          valid_indices <- which(!is.na(row))
          if (length(valid_indices) > 30) {
            top_30 <- order(row[valid_indices], decreasing = TRUE)[1:30]
            valid_indices <- valid_indices[top_30]
          }
          S_sorted[i, valid_indices] <- row[valid_indices]
        }
        return(S_sorted)
      }
      
      S_new <- S_top_30(S)
      
      # Create a function named myIBCF
      myIBCF <- function(newuser, S) {
        S <- as.matrix(S)
        n_movies <- ncol(S)
        
        preds <- rep(NA, n_movies)
        names(preds) <- colnames(S)
        
        for (i in 1:n_movies) {
          if (is.na(newuser[i])) {
            Si <- S[i, ] 
            rated_indices <- which(!is.na(newuser))
            similar_indices <- intersect(rated_indices, which(!is.na(Si)))
            
            if (length(similar_indices) > 0) {
              similarities <- Si[similar_indices]
              user_ratings <- newuser[similar_indices]
              
              numerator <- sum(similarities * user_ratings, na.rm = TRUE)
              denominator <- sum(similarities, na.rm = TRUE)
              
              if (denominator > 0) {
                preds[i] <- numerator / denominator
              }
            }
          }
        }
        top_movies <- names(sort(preds, decreasing = TRUE, na.last = NA))[1:10]
        return(top_movies)
      }
      
      user_results = (1:10)/10
      user_predicted_ids = myIBCF(user_u1181, S_new)
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function


