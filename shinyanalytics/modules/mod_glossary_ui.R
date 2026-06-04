
glossary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::navlistPanel("Glossary",
                      widths = c(2, 6),
                      shiny::tabPanel("Gross Score",
                                      h3("Gross Score"),
                                      p(strong("Gross Score")),
                                      p("Round-level, total number of strokes taken to complete 18 holes")
                                      ),
                      shiny::tabPanel("Handicap Index",
                                      h3("Handicap Index"),
                                      p(strong("Handicap Index")),
                                      p("Official USGA metric determining a player's skill level"),
                                      p("Computed using only the 8 best Gross Scores of a player's most recent 20 officially-carded rounds, submitted to the USGA (biased measure of skill)"),
                      ),
                      shiny::tabPanel("Net Score",
                                      h3("Net Score"),
                                      p(strong("Net Score")),
                                      p("Round-level, total number of strokes taken to complete 18 holes after adjusting for strokes deducted by Handicap Index and course slope")
                      ),
                      shiny::tabPanel("Skill Estimate",
                                      h3("Skill Estimate"),
                                      p(strong("Skill Estimate")),
                                      p("Custom, stabilized estimate of scoring ability that smooths round-to-round variance"),
                                      p("Tracks long-term performance independent of course difficulty (is relative to a player's average Handicap Index)")
                      ),
                      shiny::tabPanel("FIR",
                                      h3("FIR"),
                                      p(strong("FIR")),
                                      p('Abbreviation for ', strong('F'), 'airways ', strong('I'), 'n ', strong('R'), 'egulation'),
                                      p("Hole-level, boolean (1 or 0) metric for whether the fairway was hit from the tee box on Par 4's and Par 5's")
                                      ),
                      shiny::tabPanel("FIR %",
                                      h3("FIR %"),
                                      p(strong("FIR %")),
                                      p('Abbreviation for ', strong('F'), 'airways ', strong('I'), 'n ', strong('R'), 'egulation %'),
                                      p("Round-level, percentage/average metric for how many fairways were hit from the tee box on Par 4's and Par 5's in a given round")
                      ),
                      shiny::tabPanel("GIR",
                                      h3("GIR"),
                                      p(strong("GIR")),
                                      p('Abbreviation for ', strong('G'), 'reens ', strong('I'), 'n ', strong('R'), 'egulation'),
                                      p("Hole-level, boolean (1 or 0) metric for whether the green was hit on any given hole within 2 strokes of par (e.g. 2 for Par 4's")
                      ),
                      shiny::tabPanel("GIR %",
                                      h3("GIR %"),
                                      p(strong("FIR %")),
                                      p('Abbreviation for ', strong('G'), 'reens ', strong('I'), 'n ', strong('R'), 'egulation %'),
                                      p("Round-level, percentage/average metric for how many greens were hit across all holes within 2 strokes of par (e.g. 2nd shot landed on the green of a Par 4)"),
                                      p("Metric is known to be one of the better indicators of success and correlates strongly with Gross Score")
                                      ),
                      
                      shiny::tabPanel("GIR Probability Curve",
                                      h3("GIR Probability Curve"),
                                      p(strong("GIR Probability Curve")),
                                      p("The continuous probability of achieving a GIR at a given distance, across all distances, computed by regression"),
                                      p("Peaks/maxima indicate the distance with the highest probability of hitting the green in regulation")),
                      
                      shiny::tabPanel("Up-and-Down %",
                                      h3("Up-and-Down %"),
                                      p(strong("Up-and-Down %")),
                                      p('Also known as "Scrambling %"'),
                                      p("Round-level, percentage/average metric for how many holes a player who missed the green in regulation still managed to par the hole (conversions/attempts*100)")),
                      shiny::tabPanel("Tee Club",
                                      h3("Tee Club"),
                                      p(strong("Tee Club")),
                                      p("Club used on the tee shot for a given hole")),
                      
                      
                      shiny::tabPanel("Par",
                                      h3("Par"),
                                      p(strong("Par")),
                                      p("Hole- and round-level metric, determining the average number of strokes it should take to get the ball in the hole"),
                                      p("For each hole, either a 3, 4, or 5 (6 is uncommon in the United States)"),
                                      p(br("Generally, Par 3's are ~ <= 200 yards from the tee box to the middle of the green"),
                                        p("Par 4's are ~ 300 - 400 yards from the tee box to the middle of the green"),
                                        p("Par 5's are ~ 480+ yards from the tee box to the middle of the green")
                                      ),
                                      p('For each round, the sum of all "hole par"s, varying around 70-72 for most courses'),
                                      p('"Par" also serves as the term for when a player makes the ball in the hole in the number of par strokes on a given hole (e.g. 4 strokes on a Par 4: "Par"')),
                      
                      shiny::tabPanel("Birdie",
                                      h3("Birdie"),
                                      p(strong("Birdie")),
                                      p("Hole-level metric, indicating when a player makes the ball in the hole in one stroke fewer than par on a given hole")),
                      
                      shiny::tabPanel("Bogey",
                                      h3("Bogey"),
                                      p(strong("Bogey")),
                                      p("Hole-level metric, indicating when a player makes the ball in the hole in one stroke more than par on a given hole")),
                      
                      shiny::tabPanel("Double Bogey",
                                      h3("Double Bogey"),
                                      p(strong("Double Bogey")),
                                      p("Hole-level metric, indicating when a player makes the ball in the hole in two strokes more than par on a given hole")),
                      
                      shiny::tabPanel("Eagle",
                                      h3("Eagle"),
                                      p(strong("Eagle")),
                                      p("Hole-level metric, indicating when a player makes the ball in the hole in two strokes fewer than par on a given hole"))
  )
}