anthro_UI <- function() {
  list(
    tags$head(
      tags$meta(
        `http-equiv` = "Content-Security-Policy",
        content = "default-src 'none';
               script-src 'self' 'unsafe-eval' 'sha256-6nkcon48waJesjWqHozM5hGdLKBmBNxM4NFsymzrU6Y=' 'sha256-Km/NKuINXioxv4d2Ky5P909XaCTjYlOnZwI3NoSeEc4=' 'sha256-JDYsFFqB4eL9lRhcQwDSWVr7LK3Z8VgMLdzpW8GbIIQ=' 'sha256-QaAocr5Uj0pegNbn6+vFJoRKICNbyotHJaD9VhmQA8E=' 'sha256-XpcQX/3Ywnkt9EgPiYRL8q1XjqvghrOMQIBnuNiO0C0=' 'sha256-eqBgjmwYRdf5DyVLPGhbNzhOgutWk7XGmxW82LT4ZzM=' 'sha256-YE8RNDJw6U1PgYf5+Ae0kI0ClWiUxeT1BI/dfRx0njs=' 'sha256-Zt/6p8YVqUGOut2PyWl2+4RvCXYgh7DH/zxVx68zkFE=' 'sha256-ttJKrJNKz7ymhtvME370OBAbNwc7XlMAzmHGG3leKmo=' 'sha256-isWglOoW40JHXjea22+zqtkz5acJmon+tDxOAzfnW4M=' 'sha256-lGIkE3SMrCzc4m1XNSiFifWwhSBpulRLxJZi6pN+mEE=' 'sha256-aXpb0/CGUfvAm6XvbDmzPg8NaqQB5H1HFkJ4z4TN7pU=' 'sha256-mIwXfr4fo3l0pLB6NmqpaNTIkf4ZIgSU0qK3cB3t48o=' 'sha256-GmC54/KjxDlmgmIpgOs9/MqgHW9nES2ziXHM5CmimBA=' 'sha256-CfRM/Kdr/CmnA+QfzvsSJSkuLxv09JjwgVT+rpCL3s0=' 'sha256-qN6vCyFDsVdYDJ+KJBIM7n6yobZD6u0dYqNy4U71Vm8=' 'sha256-VFP54tcHzEU5nOOgbmDLBk1sEA+JC522GwqgmQT1c38=' 'sha256-3qyF6kWY8fk0AAbELBvi0hR6oZ0JcNKko4jw18mQHIc=' 'sha256-CXfm0Fq5UJ5MYZ6kxAXuHNySXImHBtoYXs7BoafHd0Q=' 'sha256-Or9nKfclkzXy+Qs74Ncow0vTJ3t/lQJUy3cZzM2yjIY=' 'sha256-aWzB4P78JSlJQPpU6jsxQ0iZhMJMMerS0NwLsTPK36A=' 'sha256-DiOXoTwxysY/p6mCaMFek/cgVVW2xis7beHYjl9R8Ws=' 'sha256-1TPYfIdHccePeJW/uvh6P6ubWSQrZYBs6o34RcqX5XE=' 'sha256-q+IXUCsEBhRv4DoV762VlqMP674accpwNtHLUh6w4Jk=' 'sha256-sH2N3FjiKBjv9noDJvrTqlp9wP8g5Y6o2vMnR7D2FzY=' 'sha256-FubxRtc8W5cIJ32bbDVEKQKvQi3hID2sgBnEa+J3J04=' 'sha256-56YZL7Xoz9E9V/nVeTM0A5R95Md4Z6WR3VY0RKESh9I=';
               style-src 'self' 'unsafe-inline';
               base-uri 'self';
               img-src 'self' data:;
               frame-src 'self';
               font-src 'self';
               connect-src 'self'"
      ),
      tags$link(rel = "stylesheet", href = "style.css")
    ),
    useBusyIndicators(),
    shinyjs::useShinyjs(),
    navbarPage(
      title = a(
        href = "http://www.who.int/nutrition/en/",
        target = "_blank",
        shiny::includeHTML("static/who-logo.svg")
      ),
      id = "mainNavBar",
      windowTitle = "WHO Anthro Survey Analyser",
      collapsible = TRUE,
      inverse = TRUE,
      tabPanel(
        "Home",
        shiny::includeHTML("static/home.html"),
        div(
          class = "row",
          div(
            class = "col-md-6 col-md-offset-3 text-center",
            hr(),
            actionButton(
              "gotoanthro",
              class = "btn btn-success",
              label = "Go to the Anthro Survey Analyser"
            )
          )
        ),
        p("")
      ),
      tabPanel(
        "Analyser",
        value = "analyser",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            id = "nav-sidebar",
            UploadInput("upload"),
            uiOutput("panel_side")
          ),
          mainPanel(
            width = 10,
            validatorOutput("data_input_validation"),
            validatorOutput("global_validation"),
            uiOutput("panel_main")
          )
        )
      ),
      navbarMenu(
        "About",
        tabPanel(
          "FAQs and Quick Guide",
          shiny::includeHTML("static/user-manual.html")
        ),
        tabPanel(
          "Software",
          div(
            class = "row",
            div(
              class = "col-md-6 col-md-offset-3",
              h1("Software"),
              p(
                "The application was developed using ",
                a(
                  href = "https://www.r-project.org/",
                  "R: A Language and Environment for Statistical Computing"
                ),
                "and ",
                a(
                  href = "https://cran.r-project.org/package=shiny",
                  "shiny: Web Application Framework for R"
                ),
                "."
              ),
              p(
                "In addition to shiny, the application depends on a number of other R packages."
              )
            )
          )
        ),
        tabPanel(
          "License",
          shiny::includeHTML("static/license.html")
        ),
        tabPanel(
          "Feedback",
          shiny::includeHTML("static/feedback.html")
        )
      )
    )
  )
}
