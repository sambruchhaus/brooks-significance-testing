{
 "cells": [
  {
   "cell_type": "markdown",
   "id": "b53f824a",
   "metadata": {
    "papermill": {
     "duration": 0.005355,
     "end_time": "2023-04-30T22:19:45.411620",
     "exception": false,
     "start_time": "2023-04-30T22:19:45.406265",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "## Load Necessary Libraries"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 1,
   "id": "4c15370e",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:45.424721Z",
     "iopub.status.busy": "2023-04-30T22:19:45.422618Z",
     "iopub.status.idle": "2023-04-30T22:19:46.447541Z",
     "shell.execute_reply": "2023-04-30T22:19:46.445751Z"
    },
    "papermill": {
     "duration": 1.034102,
     "end_time": "2023-04-30T22:19:46.450000",
     "exception": false,
     "start_time": "2023-04-30T22:19:45.415898",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "# load data acquisition libraries\n",
    "library(hoopR)\n",
    "\n",
    "# load data manipulation libraries\n",
    "library(dplyr, warn.conflicts=FALSE)\n",
    "library(tidyr)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "5a6bb734",
   "metadata": {
    "papermill": {
     "duration": 0.004148,
     "end_time": "2023-04-30T22:19:46.458563",
     "exception": false,
     "start_time": "2023-04-30T22:19:46.454415",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "## Analyze Whether Brooks Hurts the Team"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "6f5b44c3",
   "metadata": {
    "papermill": {
     "duration": 0.004027,
     "end_time": "2023-04-30T22:19:46.466665",
     "exception": false,
     "start_time": "2023-04-30T22:19:46.462638",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "### Analyze Grizzlies' Winning Percentage based on Brooks' Status"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "b4025e4d",
   "metadata": {
    "papermill": {
     "duration": 0.00436,
     "end_time": "2023-04-30T22:19:46.475173",
     "exception": false,
     "start_time": "2023-04-30T22:19:46.470813",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Retrieve Game Data"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "d6322d82",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:46.528715Z",
     "iopub.status.busy": "2023-04-30T22:19:46.485323Z",
     "iopub.status.idle": "2023-04-30T22:19:52.736791Z",
     "shell.execute_reply": "2023-04-30T22:19:52.734462Z"
    },
    "papermill": {
     "duration": 6.259951,
     "end_time": "2023-04-30T22:19:52.739268",
     "exception": false,
     "start_time": "2023-04-30T22:19:46.479317",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "# create tibble of games that dillon brooks played in\n",
    "brooks_played_tb <- load_nba_player_box(seasons = 2018:2023)%>%\n",
    "    filter(athlete_id == 3155526,\n",
    "           minutes > 0) %>%\n",
    "    drop_na(minutes) %>%\n",
    "    transmute(game_id,\n",
    "              brooks_played = TRUE)\n",
    "\n",
    "# create tibble of grizzlies game during that time period\n",
    "grizz_games_tb <- load_nba_team_box(seasons = 2018:2023) %>%\n",
    "    filter(team_id == 29)\n",
    "\n",
    "# merge tibbles\n",
    "brooks_game_analysis_tb <- left_join(grizz_games_tb,\n",
    "                                     brooks_played_tb,\n",
    "                                     by = \"game_id\") %>%\n",
    "    mutate(brooks_played = replace_na(brooks_played, FALSE))"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "a10813ec",
   "metadata": {
    "papermill": {
     "duration": 0.004031,
     "end_time": "2023-04-30T22:19:52.747611",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.743580",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Calculate Grizzlies' Winning Percentage based on Brooks' Status"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "id": "c924a109",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:52.759834Z",
     "iopub.status.busy": "2023-04-30T22:19:52.758186Z",
     "iopub.status.idle": "2023-04-30T22:19:52.861632Z",
     "shell.execute_reply": "2023-04-30T22:19:52.859938Z"
    },
    "papermill": {
     "duration": 0.112379,
     "end_time": "2023-04-30T22:19:52.864012",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.751633",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A tibble: 2 × 3</caption>\n",
       "<thead>\n",
       "\t<tr><th scope=col>brooks_played</th><th scope=col>num_games</th><th scope=col>win_pct</th></tr>\n",
       "\t<tr><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><td>FALSE</td><td>127</td><td>0.5039370</td></tr>\n",
       "\t<tr><td> TRUE</td><td>359</td><td>0.4874652</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A tibble: 2 × 3\n",
       "\\begin{tabular}{lll}\n",
       " brooks\\_played & num\\_games & win\\_pct\\\\\n",
       " <lgl> & <int> & <dbl>\\\\\n",
       "\\hline\n",
       "\t FALSE & 127 & 0.5039370\\\\\n",
       "\t  TRUE & 359 & 0.4874652\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A tibble: 2 × 3\n",
       "\n",
       "| brooks_played &lt;lgl&gt; | num_games &lt;int&gt; | win_pct &lt;dbl&gt; |\n",
       "|---|---|---|\n",
       "| FALSE | 127 | 0.5039370 |\n",
       "|  TRUE | 359 | 0.4874652 |\n",
       "\n"
      ],
      "text/plain": [
       "  brooks_played num_games win_pct  \n",
       "1 FALSE         127       0.5039370\n",
       "2  TRUE         359       0.4874652"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "# create tibble of counts for significance testing\n",
    "brooks_game_analysis_counts_tb <- brooks_game_analysis_tb %>%\n",
    "    group_by(brooks_played, team_winner) %>%\n",
    "    count() %>% \n",
    "    pivot_wider(id_cols = brooks_played,\n",
    "                names_from = team_winner,\n",
    "                values_from = n) %>%\n",
    "    rename(lost = `FALSE`,\n",
    "           won = `TRUE`) %>%\n",
    "    ungroup()\n",
    "\n",
    "# create tibble of win percentages\n",
    "brooks_game_analysis_counts_tb %>%\n",
    "    transmute(brooks_played,\n",
    "              num_games = (lost+won),\n",
    "              win_pct = won / (lost + won))"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "c6fdf601",
   "metadata": {
    "papermill": {
     "duration": 0.004339,
     "end_time": "2023-04-30T22:19:52.872984",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.868645",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Test Significance of Difference\n",
    "\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "id": "f2cadf78",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:52.885565Z",
     "iopub.status.busy": "2023-04-30T22:19:52.883497Z",
     "iopub.status.idle": "2023-04-30T22:19:52.911845Z",
     "shell.execute_reply": "2023-04-30T22:19:52.909887Z"
    },
    "papermill": {
     "duration": 0.03727,
     "end_time": "2023-04-30T22:19:52.914529",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.877259",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "\t2-sample test for equality of proportions with continuity correction\n",
       "\n",
       "data:  brooks_game_analysis_counts_tb$won out of rowSums(brooks_game_analysis_counts_tb)\n",
       "X-squared = 0.058696, df = 1, p-value = 0.4043\n",
       "alternative hypothesis: greater\n",
       "95 percent confidence interval:\n",
       " -0.07237015  1.00000000\n",
       "sample estimates:\n",
       "   prop 1    prop 2 \n",
       "0.5039370 0.4861111 \n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "prop.test(x = brooks_game_analysis_counts_tb$won,\n",
    "          n = rowSums(brooks_game_analysis_counts_tb),\n",
    "          alternative = 'greater')"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "1d41a594",
   "metadata": {
    "papermill": {
     "duration": 0.004407,
     "end_time": "2023-04-30T22:19:52.924654",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.920247",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "### Analyze Grizzlies' Net Rating based on Brooks' Status"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "ad12a0f3",
   "metadata": {
    "papermill": {
     "duration": 0.004481,
     "end_time": "2023-04-30T22:19:52.933629",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.929148",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Retrieve Data"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "id": "6d0d2105",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:52.947432Z",
     "iopub.status.busy": "2023-04-30T22:19:52.945499Z",
     "iopub.status.idle": "2023-04-30T22:19:53.013694Z",
     "shell.execute_reply": "2023-04-30T22:19:53.011891Z"
    },
    "papermill": {
     "duration": 0.077954,
     "end_time": "2023-04-30T22:19:53.016271",
     "exception": false,
     "start_time": "2023-04-30T22:19:52.938317",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "# retrieve grizzlies statistics\n",
    "grizz_game_stats_tb <- read.csv('/kaggle/input/pbpstats-mem-17-23/pbpstats_mem_scoring.csv') %>%\n",
    "    mutate(Date = as.Date(Date))\n",
    "\n",
    "# retrieve opponents statistics\n",
    "grizz_opp_game_stats_tb <- read.csv('/kaggle/input/pbpstats-mem-17-23/pbpstats_mem_scoring.csv')  %>%\n",
    "    mutate(Date = as.Date(Date))"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "60017430",
   "metadata": {
    "papermill": {
     "duration": 0.004511,
     "end_time": "2023-04-30T22:19:53.025801",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.021290",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Calculate Grizzlies' Offensive Rating based on Brooks' Status"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "id": "8e87ba35",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:53.038139Z",
     "iopub.status.busy": "2023-04-30T22:19:53.036683Z",
     "iopub.status.idle": "2023-04-30T22:19:53.102318Z",
     "shell.execute_reply": "2023-04-30T22:19:53.099990Z"
    },
    "papermill": {
     "duration": 0.07503,
     "end_time": "2023-04-30T22:19:53.105304",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.030274",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A grouped_df: 10 × 3</caption>\n",
       "<thead>\n",
       "\t<tr><th scope=col>brooks_played</th><th scope=col>season</th><th scope=col>ast_points_per_100</th></tr>\n",
       "\t<tr><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><td> TRUE</td><td>2018</td><td>53.62795</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2019</td><td>58.58856</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2019</td><td>55.48076</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2020</td><td>62.00178</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2021</td><td>50.27714</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2021</td><td>63.67459</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2022</td><td>59.87615</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2022</td><td>63.74882</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2023</td><td>64.16193</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2023</td><td>60.73263</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A grouped\\_df: 10 × 3\n",
       "\\begin{tabular}{lll}\n",
       " brooks\\_played & season & ast\\_points\\_per\\_100\\\\\n",
       " <lgl> & <int> & <dbl>\\\\\n",
       "\\hline\n",
       "\t  TRUE & 2018 & 53.62795\\\\\n",
       "\t FALSE & 2019 & 58.58856\\\\\n",
       "\t  TRUE & 2019 & 55.48076\\\\\n",
       "\t  TRUE & 2020 & 62.00178\\\\\n",
       "\t FALSE & 2021 & 50.27714\\\\\n",
       "\t  TRUE & 2021 & 63.67459\\\\\n",
       "\t FALSE & 2022 & 59.87615\\\\\n",
       "\t  TRUE & 2022 & 63.74882\\\\\n",
       "\t FALSE & 2023 & 64.16193\\\\\n",
       "\t  TRUE & 2023 & 60.73263\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A grouped_df: 10 × 3\n",
       "\n",
       "| brooks_played &lt;lgl&gt; | season &lt;int&gt; | ast_points_per_100 &lt;dbl&gt; |\n",
       "|---|---|---|\n",
       "|  TRUE | 2018 | 53.62795 |\n",
       "| FALSE | 2019 | 58.58856 |\n",
       "|  TRUE | 2019 | 55.48076 |\n",
       "|  TRUE | 2020 | 62.00178 |\n",
       "| FALSE | 2021 | 50.27714 |\n",
       "|  TRUE | 2021 | 63.67459 |\n",
       "| FALSE | 2022 | 59.87615 |\n",
       "|  TRUE | 2022 | 63.74882 |\n",
       "| FALSE | 2023 | 64.16193 |\n",
       "|  TRUE | 2023 | 60.73263 |\n",
       "\n"
      ],
      "text/plain": [
       "   brooks_played season ast_points_per_100\n",
       "1   TRUE         2018   53.62795          \n",
       "2  FALSE         2019   58.58856          \n",
       "3   TRUE         2019   55.48076          \n",
       "4   TRUE         2020   62.00178          \n",
       "5  FALSE         2021   50.27714          \n",
       "6   TRUE         2021   63.67459          \n",
       "7  FALSE         2022   59.87615          \n",
       "8   TRUE         2022   63.74882          \n",
       "9  FALSE         2023   64.16193          \n",
       "10  TRUE         2023   60.73263          "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "# create tibble with offensive ratings for significance testing\n",
    "brooks_rating_analysis_tb <- brooks_game_analysis_tb %>%\n",
    "    select(game_id,\n",
    "           season,\n",
    "           season_type,\n",
    "           game_date,\n",
    "           brooks_played,\n",
    "           team_winner) %>%\n",
    "    left_join(grizz_game_stats_tb, by = c(\"game_date\" = \"Date\")) %>%\n",
    "    mutate(off_rat = Points / OffPoss * 100)\n",
    "\n",
    "# create tibble to view by season\n",
    "brooks_rating_analysis_tb %>%\n",
    "    group_by(brooks_played, season) %>%\n",
    "    summarise(ast_points_per_100 = mean((PtsAssisted2s + PtsAssisted3s) / OffPoss * 100), .groups = \"drop_last\") %>%\n",
    "    arrange(season)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "858b45af",
   "metadata": {
    "papermill": {
     "duration": 0.004557,
     "end_time": "2023-04-30T22:19:53.114745",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.110188",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Test Significance of Difference"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "id": "63076dd1",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:53.127483Z",
     "iopub.status.busy": "2023-04-30T22:19:53.125916Z",
     "iopub.status.idle": "2023-04-30T22:19:53.161415Z",
     "shell.execute_reply": "2023-04-30T22:19:53.157744Z"
    },
    "papermill": {
     "duration": 0.045296,
     "end_time": "2023-04-30T22:19:53.164668",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.119372",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "\tWelch Two Sample t-test\n",
       "\n",
       "data:  filter(brooks_rating_analysis_tb, brooks_played == FALSE)$off_rat and filter(brooks_rating_analysis_tb, brooks_played == TRUE)$off_rat\n",
       "t = -0.57222, df = 227.57, p-value = 0.5677\n",
       "alternative hypothesis: true difference in means is not equal to 0\n",
       "95 percent confidence interval:\n",
       " -3.017037  1.659078\n",
       "sample estimates:\n",
       "mean of x mean of y \n",
       " 110.5899  111.2689 \n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "t.test(filter(brooks_rating_analysis_tb, brooks_played == FALSE)$off_rat,\n",
    "       filter(brooks_rating_analysis_tb, brooks_played == TRUE)$off_rat,\n",
    "       var.equal=FALSE) "
   ]
  },
  {
   "cell_type": "markdown",
   "id": "ad1f85e3",
   "metadata": {
    "papermill": {
     "duration": 0.004926,
     "end_time": "2023-04-30T22:19:53.175828",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.170902",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Calculate Grizzlies' Defensive Rating based on Brooks' Status"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "id": "848f0050",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:53.189500Z",
     "iopub.status.busy": "2023-04-30T22:19:53.187838Z",
     "iopub.status.idle": "2023-04-30T22:19:53.275874Z",
     "shell.execute_reply": "2023-04-30T22:19:53.273465Z"
    },
    "papermill": {
     "duration": 0.098362,
     "end_time": "2023-04-30T22:19:53.279195",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.180833",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A grouped_df: 10 × 3</caption>\n",
       "<thead>\n",
       "\t<tr><th scope=col>brooks_played</th><th scope=col>season</th><th scope=col>def_rat</th></tr>\n",
       "\t<tr><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><td> TRUE</td><td>2018</td><td>105.2598</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2019</td><td>107.2198</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2019</td><td>105.9302</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2020</td><td>109.8904</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2021</td><td>105.0388</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2021</td><td>113.6238</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2022</td><td>114.5061</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2022</td><td>115.8423</td></tr>\n",
       "\t<tr><td>FALSE</td><td>2023</td><td>116.6900</td></tr>\n",
       "\t<tr><td> TRUE</td><td>2023</td><td>115.7497</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A grouped\\_df: 10 × 3\n",
       "\\begin{tabular}{lll}\n",
       " brooks\\_played & season & def\\_rat\\\\\n",
       " <lgl> & <int> & <dbl>\\\\\n",
       "\\hline\n",
       "\t  TRUE & 2018 & 105.2598\\\\\n",
       "\t FALSE & 2019 & 107.2198\\\\\n",
       "\t  TRUE & 2019 & 105.9302\\\\\n",
       "\t  TRUE & 2020 & 109.8904\\\\\n",
       "\t FALSE & 2021 & 105.0388\\\\\n",
       "\t  TRUE & 2021 & 113.6238\\\\\n",
       "\t FALSE & 2022 & 114.5061\\\\\n",
       "\t  TRUE & 2022 & 115.8423\\\\\n",
       "\t FALSE & 2023 & 116.6900\\\\\n",
       "\t  TRUE & 2023 & 115.7497\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A grouped_df: 10 × 3\n",
       "\n",
       "| brooks_played &lt;lgl&gt; | season &lt;int&gt; | def_rat &lt;dbl&gt; |\n",
       "|---|---|---|\n",
       "|  TRUE | 2018 | 105.2598 |\n",
       "| FALSE | 2019 | 107.2198 |\n",
       "|  TRUE | 2019 | 105.9302 |\n",
       "|  TRUE | 2020 | 109.8904 |\n",
       "| FALSE | 2021 | 105.0388 |\n",
       "|  TRUE | 2021 | 113.6238 |\n",
       "| FALSE | 2022 | 114.5061 |\n",
       "|  TRUE | 2022 | 115.8423 |\n",
       "| FALSE | 2023 | 116.6900 |\n",
       "|  TRUE | 2023 | 115.7497 |\n",
       "\n"
      ],
      "text/plain": [
       "   brooks_played season def_rat \n",
       "1   TRUE         2018   105.2598\n",
       "2  FALSE         2019   107.2198\n",
       "3   TRUE         2019   105.9302\n",
       "4   TRUE         2020   109.8904\n",
       "5  FALSE         2021   105.0388\n",
       "6   TRUE         2021   113.6238\n",
       "7  FALSE         2022   114.5061\n",
       "8   TRUE         2022   115.8423\n",
       "9  FALSE         2023   116.6900\n",
       "10  TRUE         2023   115.7497"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "# create tibble with defensive ratings for significance testing\n",
    "brooks_opp_rating_analysis_tb <- brooks_game_analysis_tb %>%\n",
    "    select(game_id,\n",
    "           season,\n",
    "           season_type,\n",
    "           game_date,\n",
    "           brooks_played,\n",
    "           team_winner) %>%\n",
    "    left_join(grizz_opp_game_stats_tb, by = c(\"game_date\" = \"Date\")) %>%\n",
    "    mutate(def_rat = Points / OffPoss * 100)\n",
    "\n",
    "# create tibble to visualize by season\n",
    "brooks_opp_rating_analysis_tb %>%\n",
    "    group_by(brooks_played, season) %>%\n",
    "    summarise(def_rat = sum(Points) / sum(OffPoss) * 100, .groups = \"drop_last\") %>%\n",
    "    arrange(season)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "a5150ec4",
   "metadata": {
    "papermill": {
     "duration": 0.004999,
     "end_time": "2023-04-30T22:19:53.289374",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.284375",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "#### Test Significance of Difference"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "id": "c889b209",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2023-04-30T22:19:53.303120Z",
     "iopub.status.busy": "2023-04-30T22:19:53.301429Z",
     "iopub.status.idle": "2023-04-30T22:19:53.484134Z",
     "shell.execute_reply": "2023-04-30T22:19:53.481771Z"
    },
    "papermill": {
     "duration": 0.19278,
     "end_time": "2023-04-30T22:19:53.487234",
     "exception": false,
     "start_time": "2023-04-30T22:19:53.294454",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "\tTwo Sample t-test\n",
       "\n",
       "data:  filter(brooks_opp_rating_analysis_tb, (brooks_played == FALSE))$def_rat and filter(brooks_opp_rating_analysis_tb, (brooks_played == TRUE))$def_rat\n",
       "t = -0.56353, df = 484, p-value = 0.5733\n",
       "alternative hypothesis: true difference in means is not equal to 0\n",
       "95 percent confidence interval:\n",
       " -3.046381  1.688422\n",
       "sample estimates:\n",
       "mean of x mean of y \n",
       " 110.5899  111.2689 \n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "t.test(filter(brooks_opp_rating_analysis_tb, (brooks_played == FALSE))$def_rat,\n",
    "       filter(brooks_opp_rating_analysis_tb, (brooks_played == TRUE))$def_rat,\n",
    "       var.equal=TRUE) "
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "4.0.5"
  },
  "papermill": {
   "default_parameters": {},
   "duration": 11.628507,
   "end_time": "2023-04-30T22:19:53.615225",
   "environment_variables": {},
   "exception": null,
   "input_path": "__notebook__.ipynb",
   "output_path": "__notebook__.ipynb",
   "parameters": {},
   "start_time": "2023-04-30T22:19:41.986718",
   "version": "2.4.0"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
