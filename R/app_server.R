#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @importFrom shinyalert shinyalert
#' @noRd
app_server <- function(input, output, session) {
sheet_id <- googledrive::drive_get("SUScreenerTest")$id

  # Your application server logic
  AUDITdata<-data.frame(matrix(NA, nrow=1,ncol = 11))
  colnames(AUDITdata)<-c("AUDIT1","AUDIT2","AUDIT3","AUDIT4","AUDIT5","AUDIT6","AUDIT7","AUDIT8","AUDIT9","AUDIT10","AUDIT11")
  CAGEcdata<-data.frame(matrix(NA, nrow=1,ncol = 4))
  colnames(CAGEcdata)<-c("CAGEc1","CAGEc2","CAGEc3","CAGEc4")


  observeEvent(input$Screenerprompt, {
    output$AUDITquestions<-renderUI({
      tagList(
        f7Block(
          f7Shadow(
            intensity = 5,
            hover = TRUE,
            f7Card(
              f7Align(h2("Part 1 of 2"), side=c("center")),
              h3("INSTRUCTIONS:"),
              h4("Please answer the following questions about your alcohol use:"),
              uiOutput("AUDIT"),
              footer = NULL,
              hairlines = F, strong = T, inset = F, tablet = FALSE)
          )
        )
      )
    })
    updateF7Tabs(session = session, id = "tabs", selected = "AUDITtab")
  })




  observeEvent(input$StartOver, {
    updateF7Tabs(session = session, id = "tabs", selected = "WelcomeTab")
    updateF7Select(inputId="AUDITitem1", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem2", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem3", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem4", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem5", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem6", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem7", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem8", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem9", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem10", selected = c("Choose a response"))
    updateF7Select(inputId="AUDITitem11", selected = c("Choose a response"))

    updateF7Tabs(session = session, id = "tabs", selected = "WelcomeTab")
    updateF7Select(inputId="CAGEcitem1", selected = c("Choose a response"))
    updateF7Select(inputId="CAGEcitem2", selected = c("Choose a response"))
    updateF7Select(inputId="CAGEcitem3", selected = c("Choose a response"))
    updateF7Select(inputId="CAGEcitem4", selected = c("Choose a response"))
    output$AUDITsummary<- renderUI({})
    output$CAGEcsummary<- renderUI({})
    updateF7Text("NavUsername", label = "Navigator ID: ", value="Enter Navigator ID number")
    output$AUDITquestions<-renderUI({})
    output$CAGEcquestions<- renderUI({})
    output$IDPASSWORD<- renderUI({})
  })







  observeEvent(input$CannabisSkipSubmit, {

    output$IDPASSWORD<- renderUI({
      tagList(
        f7AccordionItem(
          title="Enter Navigator ID and Password",
          f7Card(
            br(),
            f7Text("NavUsername", label = "Navigator ID: ", value=NULL, placeholder = "Enter Navigator ID number"),
            br(),
            br(),
            br(),
            f7Password("NavPassword", label = "Password : ", value = NULL, placeholder = "Enter Password"),
            br(),
            br(),
            br(),
            f7Button("Login", "Login", rounded = T, shadow=T, fill = T),
            hairlines = F, strong = T, inset =
              F, tablet = FALSE))

      )
    })



    output$CAGEcquestions<- renderUI({
      tagList(
        f7Block(
          f7Shadow(
            intensity = 5,
            hover = TRUE,
            f7Card(
              f7Align(h2("Part 2 of 2"), side=c("center")),
              h3("INSTRUCTIONS:"),
              h4("Over the last two weeks, how often have you been bothered by the following problems?"),
              uiOutput("CAGEc"),
              footer = NULL,
              hairlines = F, strong = T, inset = F, tablet = FALSE)
          )
        )
      )
    })



    if(input$AUDITitem1=="Choose a response" ||
       input$AUDITitem2=="Choose a response" ||
       input$AUDITitem3=="Choose a response" ||
       input$AUDITitem4=="Choose a response" ||
       input$AUDITitem5=="Choose a response" ||
       input$AUDITitem6=="Choose a response" ||
       input$AUDITitem7=="Choose a response" ||
       input$AUDITitem8=="Choose a response" ||
       input$AUDITitem9=="Choose a response" ||
       input$AUDITitem10=="Choose a response" ||
       input$AUDITitem11=="Choose a response")
    {
      shinyalert::shinyalert(title="Oops, you forgot to answer a question!",type="error")
    } else {
      AUDITdata<-data.frame(matrix(NA, nrow=1,ncol = 11))
      colnames(AUDITdata)<-c("AUDIT1","AUDIT2","AUDIT3","AUDIT4","AUDIT5","AUDIT6","AUDIT7","AUDIT8","AUDIT9","AUDIT10","AUDIT11")
      AUDITdata$AUDIT1<-input$AUDITitem1
      AUDITdata$AUDIT2<-input$AUDITitem2
      AUDITdata$AUDIT3<-input$AUDITitem3
      AUDITdata$AUDIT4<-input$AUDITitem4
      AUDITdata$AUDIT5<-input$AUDITitem5
      AUDITdata$AUDIT6<-input$AUDITitem6
      AUDITdata$AUDIT7<-input$AUDITitem7
      AUDITdata$AUDIT8<-input$AUDITitem8
      AUDITdata$AUDIT9<-input$AUDITitem9
      AUDITdata$AUDIT10<-input$AUDITitem10
      AUDITdata$AUDIT11<-input$AUDITitem11
      print(AUDITdata$AUDIT1)
      AUDITdata[,1] <- factor(unlist(AUDITdata[,c(1)]), levels = c("Never", "Monthly or less", "2-4 times a month", "2-3 times a week", "4 or more times a week"), labels = c(0,1,2,3,4))
      AUDITdata[,1]<-as.numeric(AUDITdata[,c(1)])
      AUDITdata[,1]<- AUDITdata[,1]- 1
      AUDITdata[,2] <- factor(unlist(AUDITdata[,c(2)]), levels = c("0 - 2","3 or 4", "5 or 6", "7 - 9", "10 or more"), labels = c(0,1,2,3,4))
      AUDITdata[,2]<-as.numeric(AUDITdata[,2])
      AUDITdata[,2]<-AUDITdata[,2] - 1
      AUDITdata[,3:8] <- factor(unlist(AUDITdata[,c(3:8)]), levels = c("Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), labels = c(0,1,2,3,4))
      AUDITdata[,9:10] <- factor(unlist(AUDITdata[,c(9:10)]), levels = c("No", "Yes, but not in the last year", "Yes, in the last year"), labels = c(0,2,4))
      AUDITdata[,11] <- factor(unlist(AUDITdata[,c(11)]), levels = c("Never", "Currently","In the past"), labels = c(0,2,4))
      AUDITdata<-lapply(AUDITdata,as.numeric)
      AUDITdata$AUDIT11<-ifelse(AUDITdata$AUDIT11==1,0,AUDITdata$AUDIT11)
      AUDITdata$AUDIT11<-ifelse(AUDITdata$AUDIT11==2,2,AUDITdata$AUDIT11)
      AUDITdata$AUDIT11<-ifelse(AUDITdata$AUDIT11==3,4,AUDITdata$AUDIT11)
      AUDITdata$Total_AUDIT<-AUDITdata$AUDIT1+AUDITdata$AUDIT2+AUDITdata$AUDIT3+AUDITdata$AUDIT4+AUDITdata$AUDIT5+AUDITdata$AUDIT6+AUDITdata$AUDIT7+AUDITdata$AUDIT8+AUDITdata$AUDIT9+AUDITdata$AUDIT10
      print(AUDITdata)

      exportAudit<-as.data.frame(AUDITdata)
      googlesheets4::sheet_append(data = exportAudit,
                                  ss=sheet_id,
                                  sheet="AUDIT")

      if (input$CannaSkipper == "Yes"){
        updateF7Tabs(session = session, id = "tabs", selected = "CAGEctab")
      } else {
        updateF7Tabs(session = session, id = "tabs", selected = "DoneTab")
        skippedcanna<-as.data.frame(matrix(data=NA, nrow=1,ncol=5))
        skippedcanna[1,1:5]<-c("NA")
        googlesheets4::sheet_append(data = skippedcanna,
                                    ss=sheet_id,
                                    sheet="CAGE-C")
      }


      if(AUDITdata$Total_AUDIT < 4){
        Severity<-c("Low Risk")
      }
      if(AUDITdata$Total_AUDIT > 3 && AUDITdata$Total_AUDIT < 10){
        Severity<-c("Risky")
      }
      if(AUDITdata$Total_AUDIT > 9 && AUDITdata$Total_AUDIT < 14){
        Severity<-c("Harmful")
      }
      if(AUDITdata$Total_AUDIT > 13){
        Severity<-c("Severe")
      }


      if (AUDITdata$AUDIT1>2){
        AUDITflag1<-c("Drinking more than 2-3 times a month")
      } else{AUDITflag1<-NULL}
      if (AUDITdata$AUDIT2>2){
        AUDITflag2<-c("Drinking 7 or more drinks on a typical day")
      } else{AUDITflag2<-NULL}
      if (AUDITdata$AUDIT3>2){
        AUDITflag3<-c("Drinks 5 or more drinks Weekly-to-Daily")
      } else{AUDITflag3<-NULL}
      if (AUDITdata$AUDIT4>2){
        AUDITflag4<-c("Unable to stop drinking Weekly-to-Daily in the last year")
      } else{AUDITflag4<-NULL}
      if (AUDITdata$AUDIT5>2){
        AUDITflag5<-c("Failed to do what was expected of them because of drinking Weekly-to-Daily")
      } else{AUDITflag5<-NULL}
      if (AUDITdata$AUDIT6>2){
        AUDITflag6<-c("Needed a drink to get going in the morning Weekly-to-Daily in the last year")
      } else{AUDITflag6<-NULL}
      if (AUDITdata$AUDIT7>2){
        AUDITflag7<-c("Felt guilt or remorse after drinking on a Weekly-to-Daily basis")
      } else{AUDITflag7<-NULL}
      if (AUDITdata$AUDIT8>2){
        AUDITflag8<-c("Was unable to remember what happened the night before Weekly-to-Daily in the last year")
      } else{AUDITflag8<-NULL}
      if (AUDITdata$AUDIT9>2){
        AUDITflag9<-c("Self or other injury from drinking in the past year")
      } else{AUDITflag9<-NULL}
      if (AUDITdata$AUDIT10>2){
        AUDITflag10<-c("Relative/Friend/Coworker/Health provider was concerned about drinking in the past year")
      } else{AUDITflag10<-NULL}
      if (AUDITdata$AUDIT11>0){
        AUDITflag11<-c("Currently or previously in treatment for Alcohol Use")
      } else{AUDITflag11<-NULL}

      if(is.null(AUDITflag1) == T && is.null(AUDITflag2) == T && is.null(AUDITflag3) == T && is.null(AUDITflag4) == T && is.null(AUDITflag5) == T && is.null(AUDITflag6) == T && is.null(AUDITflag7) == T && is.null(AUDITflag8) == T && is.null(AUDITflag9) == T && is.null(AUDITflag10) == T && is.null(AUDITflag11) == T){
        AUDITFlagText<-c("No AUDIT items were flagged")
      } else {AUDITFlagText<-c("Flagged AUDIT Items")}


      if(AUDITdata$Total_AUDIT < 4){
        output$ResultsInstructions0<- renderUI({
          tagList(
            hr(),
            f7Align(h2("Risk Summary"), side=c("center")),
            hr(),
            f7Align(h2("Someone using alcohol at this level is at low risk for health or social complications."), side=c("center")),
            br(),
            hr(),
            f7Align(h2("Action"), side=c("center")),
            hr(),
            f7Align(h3("Positive Health Message – describe low risk drinking guidelines "), side=c("center")),
            hr(),
          )
        })
      } else { output$ResultsInstructions0<- renderUI({})}


      if(AUDITdata$Total_AUDIT > 3 && AUDITdata$Total_AUDIT < 10){
        output$ResultsInstructions1<- renderUI({
          tagList(
            hr(),
            f7Align(h2("Risky Summary"), side=c("center")),
            hr(),
            f7Align(h3("Someone using alcohol at this level may develop health problems or existing problems may worsen."), side=c("center")),
            br(),
            hr(),
            f7Align(h2("Action"), side=c("center")),
            f7Align(h3("Brief intervention to reduce use"), side=c("center")),
            hr(),

          )
        })
      } else { output$ResultsInstructions1<- renderUI({}) }


      if(AUDITdata$Total_AUDIT > 9 && AUDITdata$Total_AUDIT < 14){
        output$ResultsInstructions2<- renderUI({
          tagList(
            hr(),
            f7Align(h2("Risk Summary"), side=c("center")),
            hr(),
            f7Align(h3("Someone using alcohol at this level has experienced negative effects from alcohol use."), side=c("center")),
            br(),
            hr(),
            f7Align(h2("Action"), side=c("center")),
            f7Align(h3("Brief Intervention to reduce or abstain and specific follow-up appointment (Brief Treatment if available)"), side=c("center")),
            hr(),

          )
        })
      } else { output$ResultsInstructions2<- renderUI({}) }


      if(AUDITdata$Total_AUDIT > 13){
        output$ResultsInstructions3<- renderUI({
          tagList(
            hr(),
            f7Align(h2("Risk Summary"), side=c("center")),
            hr(),
            f7Align(h3("Someone using alcohol at this level could benefit from more assessment and assistance."), side=c("center")),
            br(),
            hr(),
            f7Align(h2("Action"), side=c("center")),
            f7Align(h3("Brief Intervention to accept referral to specialty treatment for a full assessment"), side=c("center")),

            hr(),

          )
        })
      } else {output$ResultsInstructions3<- renderUI({}) }




      output$AUDITsummary<- renderUI({
        tagList(
          f7Card(
            f7Shadow(
              intensity = 5,
              hover = TRUE,
              f7Accordion(f7Align(h2("AUDIT Summary"), side=c("center")),
                          f7AccordionItem(title="Alcohol Risk", open=T,
                                          f7Card(
                                            hr(),
                                            f7Align(h1("Alcohol Use Risk Zone"), side=c("center")),
                                            f7Align(h2(Severity), side=c("center")),
                                            hr(),
                                            hairlines = F, strong = T, inset = F, tablet = FALSE)),

                          f7AccordionItem(title="Risk Summary & Action", open=F,
                                          f7Card(
                                            uiOutput("ResultsInstructions0"),
                                            uiOutput("ResultsInstructions1"),
                                            uiOutput("ResultsInstructions2"),
                                            uiOutput("ResultsInstructions3"),
                                            hairlines = F, strong = T, inset = F, tablet = FALSE)),

                          f7AccordionItem(title="AUDIT Score", open=F,
                                          f7Card(
                                            hr(),
                                            f7Align(h2("Total AUDIT Score"), side=c("center")),
                                            f7Align(h3(c(AUDITdata$Total_AUDIT)), side=c("center")),
                                            hr(),
                                            hairlines = F, strong = T, inset = F, tablet = FALSE)),

                          f7AccordionItem(title="View Flagged Items", open=F,
                                          f7Card(
                                            hr(),
                                            f7Align(h2(AUDITFlagText), side=c("center")),
                                            hr(),
                                            h4(AUDITflag1),
                                            h4(AUDITflag2),
                                            h4(AUDITflag3),
                                            h4(AUDITflag4),
                                            h4(AUDITflag5),
                                            h4(AUDITflag6),
                                            h4(AUDITflag7),
                                            h4(AUDITflag8),
                                            h4(AUDITflag9),
                                            h4(AUDITflag10),
                                            h4(AUDITflag11),
                                            hairlines = F, strong = T, inset = F, tablet = FALSE)),
                          br())),
            hairlines = F, strong = T, inset = F, tablet = FALSE)
        )
      })
    }

  })




  observeEvent(input$CAGEcSubmit, {

    output$IDPASSWORD<- renderUI({
      tagList(
        f7AccordionItem(
          title="Enter Navigator ID and Password",
          f7Card(
            br(),
            f7Text("NavUsername", label = "Navigator ID: ", value=NULL, placeholder = "Enter Navigator ID number"),
            br(),
            br(),
            br(),
            f7Password("NavPassword", label = "Password : ", value = NULL, placeholder = "Enter Password"),
            br(),
            br(),
            br(),
            f7Button("Login", "Login", rounded = T, shadow=T, fill = T),
            hairlines = F, strong = T, inset =
              F, tablet = FALSE))

      )
    })



    if(input$CAGEcitem1=="Choose a response" ||
       input$CAGEcitem2=="Choose a response" ||
       input$CAGEcitem3=="Choose a response" ||
       input$CAGEcitem4=="Choose a response")
    {
      shinyalert::shinyalert(title="Oops, you forgot to answer a question!",type="error")
    } else {
      CAGEcdata$CAGEc1<-input$CAGEcitem1
      CAGEcdata$CAGEc2<-input$CAGEcitem2
      CAGEcdata$CAGEc3<-input$CAGEcitem3
      CAGEcdata$CAGEc4<-input$CAGEcitem4
      CAGEcdata[] <- factor(unlist(CAGEcdata), levels = c("No","Yes"), labels = c(0, 1))
      CAGEcdata<-lapply(CAGEcdata,as.numeric)
      CAGEcdata<-as.data.frame(CAGEcdata)
      CAGEcdata$Total_CAGEc<-CAGEcdata$CAGEc1+CAGEcdata$CAGEc2+CAGEcdata$CAGEc3+CAGEcdata$CAGEc4
      print(CAGEcdata)

      googlesheets4::sheet_append(data = CAGEcdata,
                                  ss=sheet_id,
                                  sheet="CAGE-C")

      updateF7Tabs(session = session, id = "tabs", selected = "DoneTab")




      if (CAGEcdata$CAGEc1 >0){
        CAGEcflag1<-c("Have you ever felt you ought to cut down on your drug use?")
      } else {CAGEcflag1<-NULL}
      if (CAGEcdata$CAGEc2 >0){
        CAGEcflag2<-c("Have people annoyed you by criticizing your drug use?")
      } else {CAGEcflag2<-NULL}
      if (CAGEcdata$CAGEc3 >0){
        CAGEcflag3<-c(" Have you felt bad or guilty about your drug use?")
      } else {CAGEcflag3<-NULL}
      if (CAGEcdata$CAGEc4 >0){
        CAGEcflag4<-c("Have you ever had used drugs first thing in the morning to steady your nerves or to get rid of a hangover (eye-opener)?")
      } else {CAGEcflag4<-NULL}
      if (is.null(CAGEcflag1) == T && is.null(CAGEcflag2) == T && is.null(CAGEcflag3) == T && is.null(CAGEcflag4) == T){
        CAGEcFlagText<-c("No Cannabis items were flagged")
      } else {CAGEcFlagText<-c("Flagged Cannabis Items")}

      output$CAGEcsummary<- renderUI({
        tagList(
          f7Card(
            f7Shadow(
              intensity = 5,
              hover = TRUE,
              f7Accordion(f7Align(h2("Cannabis Summary"), side=c("center")),
                          f7AccordionItem(title="CAGEc Summary", open=F,
                                          f7Card(
                                            hr(),
                                            f7Align(h2("Total CAGEc Score"), side=c("center")),
                                            f7Align(h3(c(CAGEcdata$Total_CAGEc)), side=c("center")),
                                            hr(),
                                            f7Align(h2(CAGEcFlagText), side=c("center")),
                                            hr(),
                                            h4(CAGEcflag1),
                                            h4(CAGEcflag2),
                                            h4(CAGEcflag3),
                                            hairlines = F, strong = T, inset = F, tablet = FALSE)),
                          br())),
            hairlines = F, strong = T, inset = F, tablet = FALSE)
        )
      })

    }

  })



  output$DONE<- renderUI({
    tagList(
      f7Shadow(
        intensity = 5,
        hover = TRUE,
        f7Card(
          title = "",
          f7Align(h1("Thank you for completing the Project ACCESS Substance Use Screener"), side=c("center")),
          f7Align(h2("Please hand the tablet back to the Navigator"), side=c("center")),
          hairlines = F, strong = T, inset =
            F, tablet = FALSE))

    )
  })


  observeEvent(input$Login,{
    if(input$NavPassword == "access"){
      updateF7Tabs(session = session, id = "tabs", selected = "Output_tab")

    } else {
      shinyalert::shinyalert(title="Wrong Password!",type="error")
    }
  })



  output$ScreenerSummary<- renderUI({
    tagList(
      f7Card(
        f7Align(h3("Total AUDIT score"), side=c("center")),
        f7Align(h3(c(AUDITdata$Total_AUDIT)), side=c("center")),

        f7Align(h3("Total CAGEc score"), side=c("center")),
        f7Align(h3(c(CAGEcdata$Total_CAGEc)), side=c("center")),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE)
    )
  })





  output$AUDIT<-renderUI({
    tagList(
      f7Card(
        h4("How often do you have a drink containing alcohol?"),
        f7Select("AUDITitem1", NULL , choices = c("Choose a response", "Never", "Monthly or less", "2-4 times a month", "2-3 times a week", "4 or more times a week"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How many drinks containing alcohol do you have on a typical day when you are drinking?"),
        f7Select("AUDITitem2", NULL , choices = c("Choose a response", "0 - 2","3 or 4", "5 or 6", "7 - 9", "10 or more"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How often do you have four or more drinks on one occasion?"),
        f7Select("AUDITitem3", NULL , choices = c("Choose a response","Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How often during the last year have you found that you were not able to stop drinking once you had started?"),
        f7Select("AUDITitem4", NULL , choices = c("Choose a response","Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How often during the last year have you failed to do what was normally expected of you because of drinking?"),
        f7Select("AUDITitem5", NULL , choices = c("Choose a response","Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How often during the last year have you needed a first drink in the morning to get yourself going after a heavy drinking session?"),
        f7Select("AUDITitem6", NULL , c("Choose a response","Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How often during the last year have you had a feeling of guilt or remorse after drinking?"),
        f7Select("AUDITitem7", NULL , choices = c("Choose a response","Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("How often during the last year have you been unable to remember what happened the night before because of your drinking?"),
        f7Select("AUDITitem8", NULL , choices = c("Choose a response","Never","Less than monthly","Monthly","Weekly","Daily or almost daily"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("Have you or someone else been injured because of your drinking?"),
        f7Select("AUDITitem9", NULL , choices = c("Choose a response", "No", "Yes, but not in the last year", "Yes, in the last year"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("Has a relative, friend, doctor, or other health care worker been concerned about your drinking or suggested you cut down?"),
        f7Select("AUDITitem10", NULL , choices = c("Choose a response", "No", "Yes, but not in the last year", "Yes, in the last year"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Card(
        h4("Have you ever been in treatment for an alcohol problem?"),
        f7Select("AUDITitem11", NULL , choices = c("Choose a response", "Never", "Currently", "In the past"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
      hr(),

      f7Button("AUDITsubmit", "Continue")
    )
  })


  observeEvent(input$AUDITsubmit,{
    output$CannabisSkip <-renderUI({
      tagList(
        f7Card(
          h4("Have you used Cannabis (Marijuana) in the past year?"),
          f7Select("CannaSkipper", NULL , choices = c("Choose a response", "No", "Yes"), selected = NULL),
          br(),
          hr(),
          f7Button("CannabisSkipSubmit", "Next"),
          footer = NULL,
          hairlines = F, strong = T, inset = F, tablet = FALSE),


      )
    })
    updateF7Tabs(session = session, id = "tabs", selected = "CannabisSkipper")






    output$CAGEc <-renderUI({
      tagList(
        f7Card(
          h4("Have you ever felt you ought to cut down on your cannabis use?"),
          f7Select("CAGEcitem1", NULL , choices = c("Choose a response", "No", "Yes"), selected = NULL),
          footer = NULL,
          hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

        f7Card(
          h4("Have people annoyed you by criticizing your cannabis use?"),
          f7Select("CAGEcitem2", NULL , choices = c("Choose a response", "No", "Yes"), selected = NULL),
          footer = NULL,
          hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

        f7Card(
          h4("Have you felt bad or guilty about your cannabis use?"),
          f7Select("CAGEcitem3", NULL , choices = c("Choose a response", "No", "Yes"), selected = NULL),
          footer = NULL,
          hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

        f7Card(
          h4("Have you ever used cannabis first thing in the morning to steady your nerves or to get rid of a hangover (eye-opener)? "),
          f7Select("CAGEcitem4", NULL , choices = c("Choose a response", "No", "Yes"), selected = NULL),
          footer = NULL,
          hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

        f7Button("CAGEcSubmit", "Finish")
      )
    })

  })



}
