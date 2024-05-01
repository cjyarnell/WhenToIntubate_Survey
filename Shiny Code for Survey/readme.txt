This contains all of the code needed to make the Shiny-based survey interface.

If you insert your own REDCap API token and URL, then you could adapt this to make a similar survey that sends responses to REDCap.

There is also a way to send responses to Google Sheets (code not included here, but it is possible).

server + ui are the essential files for the Shiny application.

translate contains the function that translates the survey into multiple languages, according to the information in the Language Key.

country is a function that alters the default country (and the top country options) depending on the survey language.

feel free to email me if questions. christopher.yarnell@mail.utoronto.ca