#### Simple Local Data Storage Using Multiple Worksheets

This [Shiny](http://shiny.rstudio.com/) app demonstrates a way to allow the user to download user-provided inputs as an Excel workbook and upload it again when he/she later begins a new session. That way, the user can return to where he/she left off previously. 

For csv or text files, please see official functions provided by [Shiny](http://shiny.rstudio.com/) : [download](http://shiny.rstudio.com/gallery/download-file.html) and [uoload](http://shiny.rstudio.com/gallery/file-upload.html). This app instead considers downloading and uploading of data in the form of multiple-worksheet Excel file. That can be useful for storing relational databases or separating tables of analysis results from raw-user data.    

The app asks you to add data to tables by clicking "Add to Table" buttons. Once some data are stored in the tables, you can see them in the right panel and download them as an Excel workbook. Hit the "Clear" button to earase the data in the tables (or refresh the web page) and then upload the Excel file.  


The source code is found in [GitHub](https://github.com/kotamine/shiny/tree/master/user_xlsx). 



