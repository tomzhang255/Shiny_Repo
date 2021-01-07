library(shiny)
library(rhandsontable)
library(shinyalert)



# Weisfeiler-Lehman Isomorphism Test
determinePartitions <- function(labeledMatrix) {
  # first initialize all vertex labels to 1
  currVertexSignatures <- rep(1, ncol(labeledMatrix))
  names(currVertexSignatures) <- colnames(labeledMatrix)
  
  # use these data structures to perform the algorithm
  tmpMat <- labeledMatrix
  currLabelPartitions <- list()
  
  for (iteration in 1:ncol(tmpMat)) {
    # update vertex signatures <- curr + rowSums
    currVertexSignatures <- currVertexSignatures + rowSums(tmpMat)
    
    # now replace each row with these signatures (only for non-zero entries)
    for (c in 1:ncol(tmpMat)) {
      for (r in 1:nrow(tmpMat)) {
        if (tmpMat[r,c] != 0) {
          tmpMat[r,c] <- currVertexSignatures[c]
        }
      }
    }
    
    # partition vertex labels based on vertex signatures
    sortedSignatures <- cbind(as.data.frame(sort(currVertexSignatures)),
                              names(sort(currVertexSignatures)))
    names(sortedSignatures) <- c("signature", "label")
    
    newLabelPartitions <- split(sortedSignatures$label, sortedSignatures$signature)
    signaturePartitions <- split(sortedSignatures$signature, sortedSignatures$signature)
    
    names(newLabelPartitions) <- NULL
    names(signaturePartitions) <- NULL
    
    # now check label partitions - if unchanged terminate algorithm
    if (identical(currLabelPartitions, newLabelPartitions)) {
      break
    }
    currLabelPartitions <- newLabelPartitions
  }
  
  return(list(currLabelPartitions, signaturePartitions))
}



# build permutation matrix given permutation
buildPermutationMatrix <- function(permutation) {
  permMat <- matrix(0, nrow = length(permutation), ncol = length(permutation))
  for (val in 1:length(permutation)) {
    permMat[permutation[val],val] <- 1
  }
  return(permMat)
}



# helper functions
initFile1UI <- function() {
  renderUI({
    fileInput(inputId = "file1", label = "Matrix 1 (.csv)", accept = ".csv",
              buttonLabel = "Upload", placeholder = "...")
  })
}

initFile2UI <- function() {
  renderUI({
    fileInput(inputId = "file2", label = "Matrix 2 (.csv)", accept = ".csv",
              buttonLabel = "Upload", placeholder = "...")
  })
}

initCalcBtnUI <- function() {
  renderUI({
    actionButton(inputId = "calculate", label = "Calculate", class = "btn-success")
  })
}

alertMessage <- function(title, text, type) {
  shinyalert(
    title = title,
    text = text,
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = type,
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
}



ui <- fluidPage(
  useShinyalert(),
  
  fluidRow(column(12, tags$h2("Graph Isomorphism Calculator"), align = "center")),
  fluidRow(column(12, tags$h4("Tom Zhang, SLU '22", style = "color:navy"), align = "center")),
  
  tabsetPanel(
    selected = "App",
    
    tabPanel(
      title = "Info",
      fluidRow(column(11, tags$h3("Inputs", style = "color:firebrick"), offset = 1)),
      fluidRow(column(11, tags$h4('- You may input values into the adjacency matrices manually'), offset = 1)),
      fluidRow(column(11, tags$h4('- Or you could upload csv files'), offset = 1)),
      fluidRow(column(10, tags$h4("- Note: the csv files should not contain any column/row names"), offset = 2)),
      fluidRow(column(10, tags$h4("- There should only be entries of adjacency matrices"), offset = 2)),
      br(),
      fluidRow(column(11, tags$h3("Controls", style = "color:firebrick"), offset = 1)),
      fluidRow(column(11, tags$h4('- Press "Calculate" to perform the Weisfeiler-Lehman isomorphism test on the adjacency matrices'), offset = 1)),
      fluidRow(column(11, tags$h4('- Hit "Permute" to apply brute force to determine any bijections'), offset = 1)),
      fluidRow(column(11, tags$h4('- Remember to click "Reset" after manipulating the matrices'), offset = 1))
    ),
    
    tabPanel(
      title = "App",
      fluidRow(br()),
      sidebarLayout(
        
        sidebarPanel(
          width = 2,
          fluidRow(column(12, numericInput(inputId = "vertices", label = "Number of Vertices",
                                           value = 5, min = 1))),
          fluidRow(column(12, uiOutput(outputId = "file1UI"))),
          fluidRow(column(12, uiOutput(outputId = "file2UI"))),
          fluidRow(column(6, uiOutput(outputId = "calcBtnUI")),
                   column(6, actionButton(inputId = "reset", label = "Reset", class = "btn-danger")))
        ),
        
        mainPanel(
          width = 10,
          fluidRow(
            uiOutput(outputId = "cols")
          )
        )
        
      )
    )
    
  )
  
)



server <- function(input, output, session) {
  
  # reactive values
  values <- reactiveValues()
  
  # helper functions
  initHot1 <- function() {
    matrix(data = as.integer(0), nrow = input$vertices, ncol = input$vertices, byrow = TRUE,
           dimnames = list(LETTERS[1:input$vertices], LETTERS[1:input$vertices])) %>%
      rhandsontable() %>%
      hot_table(rowHeaderWidth = 25) %>%
      hot_cols(colWidths = 25)
  }
  
  initHot2 <- function() {
    matrix(data = as.integer(0), nrow = input$vertices, ncol = input$vertices, byrow = TRUE,
           dimnames = list(1:input$vertices, 1:input$vertices)) %>%
      rhandsontable() %>%
      hot_table(rowHeaderWidth = 25) %>%
      hot_cols(colWidths = 25)
  }
  
  renderHot2Init <- function() {
    renderRHandsontable({
      if (!is.null(values$file2)) {
        values$file2
      } else {
        initHot2()
      }
    })
  }
  
  # calc button ui
  output$calcBtnUI <- initCalcBtnUI()
  
  # matrix 1 rendering
  output$hot1 <- renderRHandsontable({
    if (!is.null(values$file1)) {
      values$file1
    } else {
      initHot1()
    }
    
  })
  
  # matrix 2 rendering
  output$hot2 <- renderHot2Init()
  
  # ui for column with dynamic column width
  output$cols <- renderUI({
    colWidth <- ifelse(input$vertices %in% 1:5, 2,
                       ifelse(input$vertices %in% 6:9, 3,
                              ifelse(input$vertices %in% 10:13, 4,
                                     ifelse(input$vertices %in% 14:17, 5,
                                            6))))
   
    tagList(
      column(colWidth,
             tags$h4(tags$span(style="color:DarkBlue", "Matrix 1")),
             rHandsontableOutput(outputId = "hot1")),
      column(6,
             tags$h4(tags$span(style="color:Firebrick", "Matrix 2")),
             rHandsontableOutput(outputId = "hot2"))
    )
  })
  
  # uploading file 1 will automatically save it as a reactive value
  observe({
    if (!is.null(input$file1)) {
      values$file1 <-
        read.csv(input$file1$datapath, header = FALSE) %>%
          as.matrix() %>%
          matrix(data = ., nrow = nrow(.), ncol = ncol(.),
                 dimnames = list(LETTERS[1:nrow(.)], LETTERS[1:ncol(.)])) %>%
          rhandsontable() %>%
          hot_table(rowHeaderWidth = 25) %>%
          hot_cols(colWidths = 25)
      
      updateNumericInput(session = session, inputId = "vertices", label = "Number of Vertices",
                         value = ncol(jsonlite::fromJSON(rhandsontable(
                           read.csv(input$file1$datapath, header = FALSE)
                         )$x$data)), min = 1)
    }
  })
  
  # file 2 upload conversion
  observe({
    if (!is.null(input$file2)) {
      values$file2 <-
        read.csv(input$file2$datapath, header = FALSE) %>%
        as.matrix() %>%
        matrix(data = ., nrow = nrow(.), ncol = ncol(.),
               dimnames = list(1:nrow(.), 1:ncol(.))) %>%
        rhandsontable() %>%
        hot_table(rowHeaderWidth = 25) %>%
        hot_cols(colWidths = 25)
      
      updateNumericInput(session = session, inputId = "vertices", label = "Number of Vertices",
                         value = ncol(jsonlite::fromJSON(rhandsontable(
                           read.csv(input$file2$datapath, header = FALSE)
                         )$x$data)), min = 1)
    }
  })
  
  # file 1 ui rendering
  output$file1UI <- initFile1UI()
  
  # file 2 ui
  output$file2UI <- initFile2UI()
  
  # reset button resets matrices and relevant variables
  observeEvent(input$reset, {
    # changing input$vertices will trigger a matrix-rendering update
    # to save some work...
    for (i in -1:0) {
      updateNumericInput(session = session, inputId = "vertices", label = "Number of Vertices",
                         value = input$vertices + i, min = 1)
    }
    
    values$file1 <- NULL
    values$file2 <- NULL
    
    output$file1UI <- initFile1UI()
    output$file2UI <- initFile2UI()
    
    output$calcBtnUI <- initCalcBtnUI()
    
    permuteBtnStatus <<- FALSE
    
    output$hot2 <- renderHot2Init()
  })
  
  # secondary reset
  observeEvent(input$vertices, {
    # whenever numVertices is updated, reset calc/permute button
    output$calcBtnUI <- initCalcBtnUI()
    
    permuteBtnStatus <<- FALSE
  })
  
  # helper vars for calc
  permuteBtnStatus <- FALSE
  mat1 <- NULL
  mat2 <- NULL
  result1 <- NULL
  result2 <- NULL
  
  # determine isomorphism upon clicking "calculate"
  observeEvent(input$calculate, {
    # based on Weisfeiler-Lehman Algorithm result, decide whether to update calc button
    if (!permuteBtnStatus) {
      # extract the two matrices
      # converting from input because user might update an uploaded matrix
      mat1 <<-
        input$hot1 %>%
        hot_to_r() %>%
        as.matrix() %>%
        matrix(data = ., nrow = nrow(.), ncol = ncol(.),
               dimnames = list(LETTERS[1:nrow(.)], LETTERS[1:ncol(.)]))
      
      mat2 <<-
        input$hot2 %>%
        hot_to_r() %>%
        as.matrix() %>%
        matrix(data = ., nrow = nrow(.), ncol = ncol(.),
               dimnames = list(1:nrow(.), 1:ncol(.)))
      
      # error checking (for uploads in particular)
      if (sum(mat1 %in% 0:1) != length(mat1) | sum(mat2 %in% 0:1) != length(mat2)) {
        alertMessage("Error", "All adjacency matrix entries must be either 0 or 1", "error")
      } else if (sum(diag(mat1) %in% 0) != length(diag(mat1)) |
                 sum(diag(mat2) %in% 0) != length(diag(mat2))) {
        alertMessage("Error", "Diagonal entries of adjacency matrices must be 0", "error")
      } else if (sum(c(dim(mat1), dim(mat2)) %in% ncol(mat1)) != 4) {
        alertMessage("Error", "Both must be square matrices with the same dimensions", "error")
      } else {
        
        # initiate the algorithm
        result1 <<- determinePartitions(mat1)
        result2 <<- determinePartitions(mat2)
        
        if (identical(result1[[2]], result2[[2]])) {
          alertMessage("Potentially Isomorphic", "", "success")
          
          output$calcBtnUI <- renderUI({
            actionButton(inputId = "calculate", label = "Permute", class = "btn-warning")
          })
          
          permuteBtnStatus <<- TRUE
        } else {
          alertMessage("Not Isomorphic", "", "warning")
        }
      }
    } else { # now permute button is active
      # based on partitions, use brute force to determine bijection
      labelPartitions1 <- result1[[1]]
      labelPartitions2 <- result2[[1]]
      
      bijections <- list()
      
      for (partition in 1:length(labelPartitions1)) {
        for (label in 1:length(labelPartitions1[[partition]])) {
          bijections[[labelPartitions1[[partition]][label]]] <- labelPartitions2[[partition]]
        }
      }
      
      # these are all possible vertex pairings
      bijections <- bijections[sort(names(bijections))]
      
      # these are the permutations - need to filter - keep only valid rows
      permutations <- expand.grid(bijections)
      
      rowsToKeep <- rep(FALSE, nrow(permutations))
      
      for (row in 1:nrow(permutations)) {
        perm <- as.vector(t(permutations[row,]))
        perm <- sort(perm)
        if (identical(perm, sort(colnames(mat2))) == TRUE) {
          rowsToKeep[row] <- TRUE
        }
      }
      
      permutations <- permutations[rowsToKeep,]
      validPermutation <- NULL
      
      # now test each permutation until find the valid one
      for (p in 1:nrow(permutations)) {
        permutation <- as.numeric(as.vector(t(permutations[p,])))
        permMat <- buildPermutationMatrix(permutation)
        PA1Pt <- permMat %*% mat1 %*% t(permMat)
        A2 <- mat2
        dimnames(A2) <- NULL
        
        if (all.equal(PA1Pt, A2) == TRUE) {
          validPermutation <- permutation
          break
        }
      }
      
      # check again - if permutation exists, indeed isomorphic, else, not
      if (!is.null(validPermutation)) {
        alertMessage("100% Isomorphic",
                     "Labels of matrix 2 have been updated to match matrix 1", "success")
        
        mat2NewLabels <- validPermutation
        names(mat2NewLabels) <- LETTERS[1:length(validPermutation)]
        mat2NewLabels <- sort(mat2NewLabels)
        mat2NewLabels <- names(mat2NewLabels)
        
        # re-render matrix 2
        output$hot2 <- renderRHandsontable({
          matrix(mat2, nrow = input$vertices, ncol = input$vertices, byrow = TRUE,
                 dimnames = list(mat2NewLabels, mat2NewLabels)) %>%
            rhandsontable() %>%
            hot_table(rowHeaderWidth = 25) %>%
            hot_cols(colWidths = 25)
        })
      } else {
        alertMessage("Not Isomorphic",
                     "The brute force method failed to find a valid permutation", "warning")
      }
      
    }
    
  })
  
}

shinyApp(ui, server)

