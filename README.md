This repository contains the data and R code required to replicate the URZ analysis.

The ALL_called_2023 csv files should be uplaoded and combined into a single data frame named df_called. The ump_games_link2023.csv should be uploaded and assigned to a dataframe called SCGT. The file CV_results_ALL contains cross validation results by umpire for each approach, including a "baseline" MSE where every pitch in the strike zone is predicted to be a strike, and every pitch out of the rulebook strikezone is predicted to be a ball.
