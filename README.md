
# Haskell Project - Web Data Harvesting App

## Project Overview
This is a group project consisting of 4 students. The goal is to implement a stack-based Haskell app for harvesting information from the Web and saving it in a database. The application will allow users to query the data stored in the database.

### Project Name
- **haskell-project**

## Main Tasks
1. **Choose a Data Source**: Select a website containing data of interest (e.g., football scores, weather, crime data, house prices, news items, movie details).
2. **Module Development**:
   - **Types.hs**: Define Haskell data types used in the project.
   - **Fetch.hs**: Develop a function to download documents (HTML, CSV, XML, JSON) from the web.
   - **Parse.hs**: Parse the downloaded data into the Haskell data types and generate a JSON representation of the data.
   - **Database.hs**: Create and manage database tables; implement functions to save and retrieve data using the Haskell data types.
3. **Main.hs**: Enable users to interactively create and initialize an SQLite database, download data, save to the database, and run queries.

## Development Guidelines
- Use **Stack** to create, build, and run the project.
- Comment your code using **haddock notation** for automatic documentation generation.
- Implement an extra challenging feature for the project (required for full marks).

## Reporting
- Write a 1-2 page report detailing:
  - The functionality of the app.
  - Instructions on how to run the application.
  - Justification for design choices made.
  - Explanation of the extra feature implemented and its challenges.

## Collaboration
- Use **git** for version tracking and collaboration among team members.
- Utilize QM's GitHub repository: [QM GitHub](https://github.qmul.ac.uk)

---

**Note**: This markdown file is to be included in the root of the project repository for reference and guidelines.
