const fs = require("fs");

let lineups = fs.readFileSync("lineups.in", "utf8");
let lineup_entries = lineups.split(/[(|)]/g);

let games = [] // array for unique game-period objects
let game_ids = [] // array of all game_ids

lineup_entries.forEach(element => {
    
    data = element.split(/[(,)]/g);
    let game_id = data[0].toString();
    let period = data[1];
    let player = data[2].toString();
    let team = data[3].toString();

    // make new game if it DNE
    let game = games.find((current, index) =>
    {
        return current.game_id == game_id
    });

    if(game == undefined)
    {
        game_ids.push(game_id);
        
        let newgame =  { 
                            game_id: game_id,
                            period: period,
                            team1_id: "", 
                            team2_id: "",
                            team1: [],
                            team2: [],
                        };

        games.push(newgame);
        game = newgame;
    }

    // make new game-period if it DNE
    let anothergame = games.find((current, index) =>
    {
        return current.game_id == game_id && current.period == period; 
    });

    if(anothergame == undefined)
    {
        anothergame = games.find((current, index) =>
        {
            return current.game_id == game_id; 
        });
        
        let newgame =  { 
                            game_id: game_id,
                            period: period,
                            team1_id: anothergame.team1_id, 
                            team2_id: anothergame.team2_id,
                            team1: [],
                            team2: [],
                        };

        games.push(newgame);
        game = newgame;
    }
    else
        game = anothergame

    // assign game teams
    if(game.team1_id == "" && game.team2_id == "")
        game.team1_id = team;
    else if(game.team1_id != team && game.team2_id == "" )
        game.team2_id = team;

    // assign player to team
    if(team == game.team1_id)
        game.team1.push(player);
    else if(team == game.team2_id)
        game.team2.push(player);

});

// json of all games
fs.writeFile(`jsonfiles/games.json`, JSON.stringify(game_ids), (err) =>
{
    if (err)
        console.log(err);
});


// separate json file for each game
game_ids.forEach(element =>
{
    let periods = games.filter(current => 
    {
        return current.game_id == element;
    });

    fs.writeFile(`jsonfiles/game${element}.json`, JSON.stringify(periods), (err) =>
    {
        if (err)
            console.log(err);
    });
});

