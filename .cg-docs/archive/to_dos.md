To Dos':
1. Make one environment for the whole package (use either .pipdata or .pipdataenv)
2. Review if it is better to eliminate the use of log = TRUE as an argument or an if statement in functions.
3. Avoid loops and replace them with apply functions.
4. Check if nested tryCatch is a good idea.
5. Check if it is necessary to get_wrk_release in dlw functions.
6. Create subfunctions for long functions (those with too many lines or complexity)
7. Change from dplyr to collapse or data.table
9. Review if the use of pipload::survey_id_to_vars is repeted in the whole process of the pipeline.
10. Clean the file utils.R from unused functions and check if the remaining ones are necessary.
