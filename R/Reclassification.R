fncReclassification <- function () {#based on John Fox's function for comparing models
  modelPosn <- function(model){
    if (is.null(model)) return(NULL)
    if (!(model %in% models)) NULL
    else which(model == models) - 1
  }
  defaults <- list (initial.model1 = NULL, initial.model2 = NULL,
                    initial.label = NULL, initial.customthres = "0,.10,.30,1")
  dialog.values <- getDialog ("Reclassification", defaults)  
  models <- listAllModels()
  if (length(models) < 2) {
    Message(message = gettextRcmdr("There are fewer than two models."), 
            type = "error")
    tkfocus(CommanderWindow())
    return()
  }
  initializeDialog(title = gettextRcmdr("Reclassification"))
  modelsBox1 <- variableListBox(top, models, title = gettextRcmdr("First model (pick one)"),
                                initialSelection = modelPosn(dialog.values$initial.model1))
  modelsBox2 <- variableListBox(top, models, title = gettextRcmdr("Second model (pick one)"),
                                initialSelection = modelPosn(dialog.values$initial.model2))
  labelBox <- variableListBox(top, Factors(), title=gettext("Outcome variable (pick one)", domain="R-RcmdrPlugin.ROC"),
                              initialSelection=varPosn(dialog.values$initial.label, "factor"))

  customthresVar <- tclVar(dialog.values$initial.customthres) # tab
  customthresEntry <- ttkentry(top, width = "25", textvariable = customthresVar)# tab
  customthresScroll <- ttkscrollbar(top, orient = "horizontal",
                                    command = function(...) tkxview(customthresEntry, ...))
  tkconfigure(customthresEntry, xscrollcommand = function(...) tkset(customthresScroll,
                                                                     ...))
  tkbind(customthresEntry, "<FocusIn>", function() tkselection.clear(customthresEntry))
  
  onOK <- function() {
    model1 <- getSelection(modelsBox1)
    model2 <- getSelection(modelsBox2)
    label <- getSelection(labelBox)
    customthres <- as.character(tclvalue(customthresVar))
    closeDialog()
    putDialog ("Reclassification", list (initial.model1 = model1, initial.model2 = model2, initial.label = label, initial.customthres = customthres))
    if (length(model1) == 0 || length(model2) == 0) {
      errorCondition(recall = Reclassification, message = gettextRcmdr("You must select two models."))
      return()
    }
    if (!checkMethod("anova", model1)) {
      return()
    }
    if (!class(get(model1, envir = .GlobalEnv))[1] == class(get(model2, 
                                                                envir = .GlobalEnv))[1]) {
      Message(message = gettextRcmdr("Models are not of the same class."), 
              type = "error")
      Reclassification()
      return()
    }
    if (glmP()) {
      family1 <- eval(parse(text = paste(model1, "$family$family", 
                                         sep = "")))
      family2 <- eval(parse(text = paste(model2, "$family$family", 
                                         sep = "")))
      if (family1 != family2) {
        Message(message = gettextRcmdr("Models do not have the same family."), 
                type = "error")
        Reclassification()
        return()
      }
      .activeDataSet <- ActiveDataSet()
      
      #require(PredictABEL)
      doItAndPrint(paste(.activeDataSet, "$output.temp.variable = as.numeric(", .activeDataSet, "$", label, ")-1", sep = ""))
      doItAndPrint(paste("reclassification(data=", .activeDataSet, ", cOutcome=match('output.temp.variable', names(", .activeDataSet, ")),
                     predrisk1=predRisk(", model1, "), predrisk2=predRisk(", model2, "), c(", customthres, "))", sep = ""))
      doItAndPrint(paste("rm(", .activeDataSet, "$output.temp.variable)", sep = ""))
    }
#     else doItAndPrint(paste("anova(", model1, ", ", model2, 
#                             ")", sep = ""))
    
    

    
    
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "anova", reset = "Reclassification", apply = "Reclassification")
  tkgrid(getFrame(modelsBox1), getFrame(modelsBox2), getFrame(labelBox), sticky = "nw")
  tkgrid(labelRcmdr(top, text = gettext("Cuttof values for risk categories", domain="R-RcmdrPlugin.ROC")), customthresEntry, sticky = "ew", padx=6)
  tkgrid(labelRcmdr(top, text =""), customthresScroll, sticky = "ew", padx=6)
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}