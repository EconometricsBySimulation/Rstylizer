library(shiny)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the liff.etime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {


  # Return the formula text for printing as a caption
  syntax.highlighted <- reactive({
    
    txt <- input$input_code
    
    txt <- unlist(strsplit(txt, split="\n"))
    
    # Uncomment this next line to test the code off the server
    # txt <- readLines("https://gist.github.com/EconometricsBySimulation/6235945/raw/02bb07a48d9cb0bd6d76309967be8b827506dea7/gistfile1.txt")
    
    
    # Choose the formatting tags you would like applied to each field type.
    comment.start <- '<span style="color: #669933">'
    comment.end   <- '</span>'
    
    # I would like to auto format all numbers but I have nto yet been able to figure
    # out how to do this.
    num.start <- '<span style="color: #990000"><b>'
    num.end   <- '</b></span>'
    
    punc.start <- '<span style="color: #0000FF">'
    punc.end   <- '</span>'
    
    command1.start <- '<span style="color: #0000CC"><b>'
    command1.end   <- '</b></span>'
    
    command2.start <- '<span style="color: #9900FF">'
    command2.end   <- '</span>'
    
    command3.start <- '<span style="color: #990033">'
    command3.end   <- '</span>'
    
    # I am not sure where exactly I got this 
    stata.commands1 <- unlist(strsplit(readLines(
      "Stata/C1.txt"), split=" "))
    stata.commands2 <- unlist(strsplit(readLines(
      "Stata/C2.txt"), split=" "))
    stata.commands3 <- unlist(strsplit(readLines(
      "Stata/C3.txt"), split=" "))
        
    # I want to figure out how to highlight the puncuation as well but I am having trouble
    # with that.
    # for (v in punc) txt<-  gsub(v, 
    #        paste0(punc.start,v,punc.end), txt)
    
    # Create a vector to tell R to ignore entire lines.
    comment <- (1:length(txt))*0
        
    # '*' Star comment recognizer
    for (i in grep("[:*:]", txt)) {
      # Break each line to discover is the first symbol which is not a space is a *
      txt2 <- strsplit(txt[i], split=" ")[[1]]
      if (txt2[txt2!=""][1]=="*") {
        txt.rep <- paste(c(comment.start,txt[[i]],comment.end), collapse="")
        txt[[i]] <- txt.rep
        comment[i] <- 1
      }
    }
    
    # '//' Comment recognizer
    for (i in (grep("//", txt))) if (comment[i]==0) {
      txt2 <- strsplit(txt[i], split=" ")[[1]]
      comment.place <- grep("//", txt2)[1]-1
      txt.rep <- paste(c(txt2[1:comment.place], comment.start, txt2[-(1:comment.place)],comment.end), collapse=" ")
      txt[[i]] <- txt.rep
    }
    
    # Format stata commands that match each list
    # "\\<",v,"\\>" ensures only entire word matches
    # are used.
    for (v in stata.commands1) txt[comment==0]<-
      gsub(paste0("\\<",v,"\\>"), 
           paste0(command1.start,v,command1.end), 
           txt[comment==0])
    
    for (v in stata.commands2) txt[comment==0]<-
      gsub(paste0("\\<",v,"\\>"), 
           paste0(command2.start,v,command2.end), 
           txt[comment==0])
    
    for (v in stata.commands3) txt[comment==0]<-
      gsub(paste0("\\<",v,"\\>"), 
           paste0(command3.start,v,command3.end), 
           txt[comment==0])
    
    # This is my attempt at highlighting all numbers that are not words.
    # It did not work.  
    # <a href ="http://stackoverflow.com/questions/18160131/replacing-numbers-r-regular-expression">stackoverflow topic</a>
    # txt <- gsub(".*([[:digit:]]+).*", paste0(num.start,"\\1",num.end), txt)
    
    # Add tags to the end and beginning to help control the general format.
    txt <- c('<pre><span style="font-family: monospace">',txt,
              '\nFormatted By <a href="http://www.econometricsbysimulation.com/2013/08/Rstylizer.html">Econometrics by Simulation</a>',
              '</span></pre>')
    # writeClipboard(paste(txt, collapse="\n"))
    txt
  })
  
  output$formatted <- renderText({paste(syntax.highlighted(), collapse="\n")})
    
  output$htmlformatted <- renderText({
    txt <- syntax.highlighted()
    txt <- gsub("<","&lt;",txt) 
    txt <- gsub(">","&gt;",txt) 
    
    txt <- c('<pre><span style="font-family: monospace">',txt, '</span></pre>')
    paste(txt, collapse="\n")
  })

})
