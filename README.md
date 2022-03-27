# Final case report


In your case report, your goal is to propose and employ methodology to examine / solve the client's questions of interest. 

There are two mandatory files:
1. `report.Rmd` - The Rmarkdown that generates your report.
1. `report.pdf` - The knitted pdf as produced by your `.Rmd`.

Only those two may be at the root (`./`) of your repo. Any additional files must be in subdirectories that are nicely laid out. I suggest you use subdirectories carefully, especially for long scripts (put these in, e.g. `code/01-exploration.R` rather than inside `report.Rmd`).

## Expectations

Write a client report summarizing your comments and proposal for the case. This portion is at most 3 pages (excluding any carefully chosen tables or figure), or approximately 1000-1500 words. Consult the attached rubric as a general guide (note that the rubric says 3-5 pages, but your limit is **3**).

The client report should have:

1. Summary: brief non-technical summary of the report. 
1. Introduction: brief motivation of the problem and description of the questions you address in the report (non-technical).
1. Statistical Methods (and Results): describe the data briefly and use plain English to describe your proposed analysis (and the results). Justify your choices and discuss assumptions and potential limitations. You need to describe your analysis correctly, yet using plain language for a client of a different discipline. You can use statistical words and terms but you need to make sure that they are clearly and correctly described.  
1. Conclusions
1. You should add a *Statistical Appendix* detailing your analysis. The target audience is statisticians (you and your classmates). Describe your analysis carefully using statistical language and undertaking any EDA, diagnostics, model checks, that are necessary. Carefully describe issues with data cleaning. If you do something Bayesian, describe how you know your MCMC converged. Etc. The page limit for this component is 5 pages. Think of this as a roadmap carefully describing what you did and why. If you needed to return to this project in the future, this part tells you, more precisely, what you did. 

If you want writing feedback on the client report (and you should) you must send the pdf to Estella (on Slack) at least a week before the deadline. 

---

Grade: total 15 pts

- clarity, conciseness, paragraph organization: 3 pts
- grammatical mistakes: 2 pts
- statistical depth, quality of the analysis: 5 pts 
- description of the analysis (general audience): 3 pts 
- description of the analysis, graphical and numerical summaries (statistical audience): 2 pts

