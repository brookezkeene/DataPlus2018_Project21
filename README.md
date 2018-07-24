# DataPlus2018_Project21: Co-Curricular Pathways (E-Advisor)

## Final R Shiny App &rarr; eAdvisor Folder
### https://brookezkeene.shinyapps.io/eAdvisor
**ui.R** &rarr; user interface of final Shiny app

**server.R** &rarr; data analysis and management for final Shiny app

## Initial Testing of R Shiny App
**app.R** &rarr; R script that simply creates and runs the shiny app
* testing ui.R and server.R as sources for Shiny App

**ui.R** &rarr; user interface R script that handles front-end appearance
* testing Shiny dashboard package and the use of tabs and widgets

**server.R** &rarr; server R script that handles back-end data management
* testing saving and receiving data from google sheets
* testing hybrid and jaccard recommender widgets

## Recommendation System
**ContentBasedRec.R** &rarr; our initial content-based recommender

**CollaborativeRec.R** &rarr; our initial collaborative filtering recommender
* includes user vs. user and item vs. item collaborative filtering

**JaccardRec.R** &rarr; our initial testing with Jaccard similarity
* can be used to find similar programs or students
* may use to improve collaborative or content based filtering

**RecSim.R** &rarr; R script to run and test our recommendation system

## Tagwords Search etc. in Python

**DukeGroups_ScrapeDescriptions.ipynb**: 
* Gather links of organizations in a list, then enter each link and collect all the descriptions in a list.
* Perhaps one day should scrape the Full Roster. However, the member information is not accurate.

**DukeGroups_SearchTagwords.ipynb** &rarr; Takes advantage of DukeGroups' search bar. Input all the tagwords manually we came up, and collect all co-curricular names shown in the search results. Create a dictionary whose keys are the co-curricular activity names, and whose values are 0 or 1 indicating if this co-curricular activity contains a specific tag.

**Topic_Modeling.ipynb** &rarr; include multiple methods to extract keywords from text, such as RAKE, TextBlob, and LDA. More testings are needed to ensure accuracy.

**Processing_Tagwords.ipynb** &rarr; import Tag_Words.csv and read the tagwords inside it.

**Cluster_Groups.ipynb** &rarr; clusters student programs using PCA and K-Means

**Cluster_Tagwords.ipynb** &rarr; clusters tagwords using PCA and K-Means

**Generate_Students.ipynb** &rarr; simulate student profiles using Normal Distribution

**Collavorative_Rec.ipynb** &rarr; use user-based collaborative filtering to give students recommendations.
