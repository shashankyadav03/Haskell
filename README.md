# Haskell
This is a group project. Groups should consists of 4 students for Haskell project
During this project your group will implement a stack-based Haskell app for harvesting information from the Web and saving it on a database. Make sure you are using stack to create, build and run your project. Please use "haskell-project" for the Haskell stack project name. The app should also give the users the ability to query the data on the database. The main tasks can be summarised as follows:
Choose a website which contains some data of interest. This could be anything you like, e.g. football scores, weather or crime data, house prices, news items, movie details, etc.
Write one module that defines the Haskell data types you are using (Types.hs), another defines a function for downloading the document (HTML, CSV, XML or JSON) from the web (Fetch.hs), and another module that parses the downloaded data into the given Haskell datatype (Parse.hs).
Write a module (Database.hs) that creates DB tables, saves/retrieves data from/to a database using again the appropriate Haskell data types.
Your parsing module (Parse.hs) should also provide a function that generates a JSON representation of your Haskell data and writes to a file.
From the Main.hs a user should be able to (interactively) create and initialise an sqlite database, download data and save to database, run queries on the database.
Comment your code using haddock notation so that haddock documentation can be automatically generated for your app.
Come up with an extra challenging feature related to your project and implement that feature (required for full marks).
Write a 1-2 page(s) report explaining what your app does, how to run it, and justify any design choices you have had to make. If you have implemented an extra feature, explain what that feature is, and why it was challenging to implement.
