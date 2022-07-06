# Generate summary table of index's components and directions

# ------------------------------------------------------------------------------
# Table of Main indexes, intersection of surveys 
indexes <- c("Misinformation importance index",
             "Likelihood of false of traditional sources index",
             "Likelihood of false of social media sources index",
             "Distrust traditional sources index",
             "Distrust social media sources index",
             "Attention to misinformation index",
             "Knowledge to identify information index",
             "Knowledge to verify information index",
             "Consumption behavior of traditional sources index",
             "Consumption behavior of social media sources index",
             "Sharing behavior index",
             "Verifying behavior index")

# Misinformation importance index 
vars1 <- c("How likely do you think misinformation contributes to the following problems in Bolivian society?^{1}")

# Likelihood of false index
vars2 <- c("How much do you think the information in these sources is true? - Traditional media^{2}")

vars3 <- c("How much do you think the information in these sources is true? - Social media^{2}",
           "Hypothetical scenario 1: How likely is it that you think it's fake?^{3}",
           "Hypothetical scenario 3: How likely is it that you think it's fake?^{3}")

# Trust index
vars4 <- c("How much do you trust the information you receive from these sources? - Traditional media^{2}")

vars5 <- c("How much do you trust the information you receive from these sources? - Social media^{2}")

# Attention to misinformation index
vars6 <- c("How often do you question whether a news is false")

# Knowledge to identify information index
vars7 <- c("How much knowledge do you have to identify whether a news is false or not?",
           "What are the main characteristics of a piece of news that make you doubt that it may be false?",
           "How many recent misinformation cases in Bolivia do you know?",
           "What are the main means by which fake news spread? - Traditional media^{2}",
           "What are the main means by which fake news spread? - Social media^{2}")

# Knowledge to verify information index
vars8 <- c("How much knowledge do you have to verify if doubtful news is false or not?",
           "What are the main ways to verify news?",
           "If you wanted to verify some information, what fact-checkers could you use? - Number of fact-checkers")

# Consumption behavior index
vars9 <- c("How often do you get news from these different types of sources? - Traditional media^{2}")

vars10 <- c("How often do you get news from these different types of sources? - Social media^{2}")

# Sharing behavior index
vars11 <- c("If you know a story is false, how often do you share its falsehood with others on social media?",
           "How often do you share news you receive on WhatsApp and social media",
           "Hypothetical scenario 1: How likely are you to share it without verifying it first?^{3}",
           "Hypothetical scenario 3: How likely are you to share it without verifying it first?^{3}")

# Verifying behavior index
vars12 <- c("How often do you verify news that you doubt may be false before sharing it?")

indexes <- rep(indexes, c(length(vars1), length(vars2), length(vars3), length(vars4),
                          length(vars5), length(vars6), length(vars7), length(vars8), 
                          length(vars9), length(vars10), length(vars11), length(vars12)))

vars_all <- c(vars1, vars2, vars3, vars4, vars5, vars6, vars7, vars8, vars9, vars10, vars11, vars12)


directions <- c(1,-1,1,1,1,1,-1,1,1,1,1,-1,1,1,1,1,1,-1,1,-1,-1,-1,1)
# Directions as as vectors:  "(1)", "(-1,1,1,1)", "(1,-1)", "(1)", "(1,1,1,1,-1)", "(1,1,1)", "(1,-1)", "(1,1,-1,-1)", "(1)")

df <- cbind.data.frame(indexes, vars_all, directions)

names(df) <- c("Indexes", "Variables", "Direction")

table <- kable(df, booktabs = TRUE, escape = FALSE, label = "table:WordingMain",
               caption = "Endline survey questions used to create all outcome indices", format = "latex", align = "llc", position = "H") %>%  
  kable_styling(font_size = 8) %>%
  row_spec(0,bold=TRUE) %>%
  column_spec(1, bold = TRUE, width = "10em") %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

# Append notes to table above
note <- "$^1$ Problems in Bolivian society: Decisions that may affect health, the election of candidates who do not represent the interests of citizens,  disparage or exalt people, ideological polarization, violence towards certain people or falsely from society, and increase hatred towards certain people or groups in society. \n
$^2$ Traditional media: Radio/TV, Newspaper, and webpages. Social media: WhatsApp, Conversations with family and friends, WhatsApp with family and friends, and WhatsApp group with unknown people.
$^3$ Hypothetical scenario 1: You received an image that came to you on WhatsApp with a screenshot of the Twitter account of a well-known person saying something very controversial.  \n
Hypothetical scenario 3: You received an audio that came to you by WhatsApp reporting evidence of corruption of a politician that you already suspected and you did not like. "

table <- add_notes(table, note)

# Save table
cat(left_align(table, 1), file = 'Tables/Appendix/IndexesVariables_main.txt')


# Table of Union of both surveys -----------------------------------------------
indexes <- c("Misinformation importance index",
             "Likelihood of false of traditional sources index",
             "Likelihood of false of social media sources index",
             "Trust traditional sources index",
             "Distrust social media sources index",
             "Attention to misinformation index",
             "Knowledge to identify information index",
             "Knowledge to verify information index",
             "Consumption behavior of traditional sources index",
             "Consumption behavior of social media sources index",
             "Sharing behavior index",
             "Verifying behavior index")


# Misinformation importance index 
vars1 <- c("How likely do you think misinformation contributes to the following problems in Bolivian society?^{1}",
           "Of the following topics, which are you most interested in the news? - Misinformation")

# Likelihood of false index
vars2 <- c("How much do you think the information in these sources is true? - Traditional media^{2}")

vars3 <- c("How much do you think the information in these sources is true? - Social media^{2}",
           "Hypothetical scenario 1: How likely is it that you think it's fake?^{3}",
           "Hypothetical scenario 3: How likely is it that you think it's fake?^{3}")

# Trust index
vars4 <- c("How much do you trust the information you receive from these sources? - Traditional media^{2}")

vars5 <- c("How much do you trust the information you receive from these sources? - Social media^{2}")

# Attention to misinformation index
vars6 <- c("How often do you question whether a news is false",
           "Avoid misinformation by being suspicious of all the information", 
           "Avoid misinformation by paying close attention")

# Knowledge to identify information index
vars7 <- c("How much knowledge do you have to identify whether a news is false or not?",
           "What are the main characteristics of a piece of news that make you doubt that it may be false?",
           "How many recent misinformation cases in Bolivia do you know?",
           "What are the main means by which fake news spread? - Traditional media^{2}",
           "What are the main means by which fake news spread? - Social media^{2}")

# Knowledge to verify information index
vars8 <- c("How much knowledge do you have to verify if doubtful news is false or not?",
           "What are the main ways to verify news?",
           "If you wanted to verify some information, what fact-checkers could you use? - Number of fact-checkers")

# Consumption behavior index
vars9 <- c("How often do you get news from these different types of sources? - Traditional media^{2}")

vars10 <- c("How often do you get news from these different types of sources? - Social media^{2}")

# Sharing behavior index
vars11 <- c("If you know a story is false, how often do you share its falsehood with others on social media?",
           "How often do you share news you receive on WhatsApp and social media",
           "Hypothetical scenario 1: How likely are you to share it without verifying it first?^{3}",
           "Hypothetical scenario 3: How likely are you to share it without verifying it first?^{3}")

# Verifying behavior index
vars12 <- c("How often do you verify news that you doubt may be false before sharing it?",
           "What are the best ways to avoid being fooled by fake news circulating on WhatsApp and social networks",
           "Hypothetical scenario 1: How likely are you to verify it?^{3}",
           "Hypothetical scenario 3: How likely are you to verify it?^{3}")

indexes <- rep(indexes, c(length(vars1), length(vars2), length(vars3), length(vars4),
                          length(vars5), length(vars6), length(vars7), length(vars8), 
                          length(vars9), length(vars10), length(vars11), length(vars12)))

vars_all <- c(vars1, vars2, vars3, vars4, vars5, vars6, vars7, vars8, vars9, vars10, vars11, vars12)

survey <- c("Both", "WhatsApp", 
            "Both", 
            "Both", "Both", "Both", 
            "Both", 
            "Both", 
            "Both", "WhatsApp", "WhatsApp",
            "Both", "Both", "Both", "Both", "Both",
            "Both", "Both", "Both", 
            "Both", 
            "Both",
            "Both", "Both", "Both", "Both",
            "Both", "WhatsApp", "Course", "Course")

directions <- c(1,1,-1,1,1,1,1,-1,1,1,1,1,1,1,-1,1,1,1,1,1,-1,1,-1,-1,-1,1,1,1,1)
# Directions as as vectors:  "(1,1)", "(-1,1,1,1)", "(1,-1)", "(1,1,1)", "(1,1,1,1,-1)", "(1,1,1)", "(1,-1)", "(1,1,-1,-1)", "(1,1,1,1)")

df <- cbind.data.frame(indexes, vars_all, survey, directions)

names(df) <- c("Indexes", "Variables", "Survey", "Direction")

table <- kable(df, booktabs = TRUE, escape = FALSE, label = "table:WordingMainUnion",
               caption = "Endline survey questions used to create all outcome indices", format = "latex", align = "lllc", position = "H") %>%  
  kable_styling(font_size = 7) %>%
  row_spec(0,bold=TRUE) %>%
  column_spec(1, bold = TRUE, width = "10em") %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

# Append notes to table above

note <- "$^1$ Problems in Bolivian society: Decisions that may affect health, the election of candidates who do not represent the interests of citizens,  disparage or exalt people, ideological polarization, violence towards certain people or falsely from society, and increase hatred towards certain people or groups in society. \n
$^2$ Traditional media: Radio/TV, Newspaper, and webpages. Social media: WhatsApp, Conversations with family and friends, WhatsApp with family and friends, WhatsApp group with unknown people, and WhatsApp organizations you are subscribed to - 'WhatsApp from organizations you subscribe to' category only appears in the WhatsApp survey. \n
$^3$ Hypothetical scenario 1: You received an image that came to you on WhatsApp with a screenshot of the Twitter account of a well-known person saying something very controversial.  \n
Hypothetical scenario 3: You received an audio that came to you by WhatsApp reporting evidence of corruption of a politician that you already suspected and you did not like. "

table <- add_notes(table, note)

# Save table
cat(left_align(table, 1), file = 'Tables/Appendix/IndexesVariables_Union.txt')


