# Values-based Repository Assessment
An interactive data repository assessment questionnaire based on values and metrics you select. 
The tool provides an overall assessment score based on the provided metrics and a downloadable report.

## Local use
The tool can be run offline

- Install [RStudio](https://posit.co/download/rstudio-desktop/) and [install](https://www.dataquest.io/blog/install-package-r/) the packages listed in lines 10-13 in `app.R`
- Clone this repo (or download manually by clicking the green Code button above and selecting Download Zip(the code)
- Open the `app.R` in RStudio
- Comment out the `GET` command near the top of `app.R` to disable overwriting the assessment rubric that was downloaded as part of the Clone step
- Modify `rubric.xslx` to suit your needs

## Rubric
The rubric is organized into Activities each with an associated Value, Advice, and evaluation metric. To add new Activities and Metrics, add a new row. **All Metrics associated with an Activity must be grouped in contiguous rows**. See `rubric.xslx` for an example. The colors in `rubric.xlsx` are only to visually group the different Activities to make editing easier.

Column name    |  Description
---------------|-------------
`Activity`     | The data repository Activity that is being evaluated
`Description`  | Description of the Activity
`Value`        | The value that is being evaluated
`Advice`       | Considerations, questions, or indicators associated with the Value
`Metric`       | The corresponding metric to evaluate
`Score n`      | Scoring criteria for the Metric. A criterion under Score 0 will contribut zero points towards the assessment score, Score 1 one point etc. Currently `n=0..3`
`Metric weight`| If certain Metrics are more important than others, adjust their weight. The metric Score will be multiplied by the weight when calculating the assessment score

## Pen-and-paper Usage
The rubric can be used by directly answering the questions in the spreadsheet. Enter the score for a particular metric in the `Selected Option` column and the results will be automatically tabulated in the `Weighted Score` column.

## Web Deployment
To make the tool publicly accessible, it needs to be hosted. Posit Connect Cloud [(formerly shinyapps.io)](https://docs.posit.co/shinyapps.io/guide/migration/) is free and recommended. Refer to Posit's documentation for how to deploy using that platform.

