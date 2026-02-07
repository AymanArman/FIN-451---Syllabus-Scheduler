library(jsonlite)

course_data <- jsonlite::fromJSON("./web_scraper/courses.json")

# https://apps.ualberta.ca/catalogue/course/fin/201 change course number at the end of this link and loop dynamically to scrape each number from above df ^
# Search for: <div class="alert alert-warning" role="alert"> to check if course is offered (or look for text / use xpath) if exists then move on otherwise scrape syllabus


# Scrape class IDs (ex (81415)) +> regex to remove bracket, then add each ID to end of syllabus download button  after 1940 (href="/catalogue/syllabus/download/194081415">

# after download, convert to pngs and the pngs woud pull into server then text would be extracted.

