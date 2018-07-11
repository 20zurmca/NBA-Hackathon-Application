The solution to Q1 was generated with SQL and JavaScript (NodeJS).

Firstly, all the given data was imported into an SQL database for
cleanup. The rationale and code for that process is in relevant_plays.sql.
The file relevant_plays.csv/in was produced and it has all the relevant plays
to computing plus/minus in game, period and chronological order, with the 
adjustment that all substitutions occuring between free throws were moved
to the end of the set of free throws in question.

games.js was used to read in lineups from lineups.in and game-period objects
were made for each period of a game. These include information of the teams 
and players involved and json files for each game were produced. A json file
of all the game_ids was also produced.

plays.js was used to read in plays from relevant_plays.in and play objects
were made for each play with information such as the score on that play, or 
the player substituting in or out etc. json files of all the plays for each
game were produced. 

For each game_id, solution.js read in the relevant file for its plays and
lineups. For each game, the program kept an object to track all active players
and accounted for scores and substitutions on each play. The solution was
produced from this program.

Note that all the intermediary json files were removed for this submission.
To reproduce the solution, install node, install the fs package and run games.js,
plays.js and solution.js in that order. Delete jsonfiles directory afterwards.