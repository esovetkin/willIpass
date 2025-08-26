ui <- fluidPage(
    tags$head(
             tags$title("Bestehenswahrscheinlichkeit der Theorieprüfung"),
             tags$style(HTML("
     .wrapped-text pre {
        white-space: pre-wrap;       /* wrap text */
        word-wrap: break-word;       /* break long words */
      }")),
             tags$script(HTML("
    // --- tiny cookie helpers ---
    function setCookie(name, value, days) {
      var d = new Date();
      d.setTime(d.getTime() + (days*24*60*60*1000));
      var expires = 'expires=' + d.toUTCString();
      document.cookie = name + '=' + encodeURIComponent(value) + ';' + expires + ';path=/;SameSite=Lax';
    }
    function getCookie(name) {
      var cname = name + '=';
      var decoded = decodeURIComponent(document.cookie || '');
      var parts = decoded.split(';');
      for (var i=0; i<parts.length; i++) {
        var c = parts[i].trim();
        if (c.indexOf(cname) === 0) return c.substring(cname.length);
      }
      return '';
    }

    var COOKIE_NAME = 'willIpass_v1';

    // Fill the input ASAP on page load (even before Shiny finishes connecting)
    document.addEventListener('DOMContentLoaded', function() {
      var v = getCookie(COOKIE_NAME);
      if (v) {
        var el = document.getElementById('numbers');
        if (el && !el.value) el.value = v; // visually prefill
      }
    });

    // When Shiny connects, also push the value into Shiny's input system
    document.addEventListener('shiny:connected', function() {
      var v = getCookie(COOKIE_NAME);
      if (v) {
        // update Shiny input value so server sees it immediately
        Shiny.setInputValue('numbers', v, {priority: 'event'});
        // also provide a dedicated signal if you use it server-side
        Shiny.setInputValue('numbers_from_cookie', v, {priority: 'event'});
      }
    });

    // Server can call this to update the cookie whenever the text changes
    Shiny.addCustomMessageHandler('saveCookie', function(val) {
      setCookie(COOKIE_NAME, val ?? '', 365);
    });
  "))
  ),
  titlePanel("Bestehenswahrscheinlichkeit der Theorieprüfung"),
  sidebarLayout(
      sidebarPanel(
          helpText("z.B.: 2, 2, 0, 5, 5"),
          textInput(
              "numbers",
              label = "Ergebnisse der Prüfungssimulationen (kommas getrennten Zahlen)",
              placeholder = "z.B.: 2, 2, 0, 5, 5"
          ),
          actionButton("compute", "Berechnen", class = "btn-primary"),
          br(), br(),
          uiOutput("parse_notes")
      ),
      mainPanel(
          div(class = "wrapped-text",
              verbatimTextOutput("summary")
              ),
          h4("Verteilungsfunktion mit einem 90%-Konfidenzintervall"),
          plotOutput("freq_plot", height = 360)
      )
  )
)
