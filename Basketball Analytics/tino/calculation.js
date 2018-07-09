const fs = require("fs");

let lineups = fs.readFileSync("lineups.txt", "utf8");
let lineup_entries = lineups.split(/[(?:|)]/g);

let games = [] // array for unique game-period objects

lineup_entries.forEach(element => {
    
    data = element.split(/[(?:,)]/g);
    let game_id = data[0];
    let period = data[1];
    let player = data[2];
    let team = data[3];

    // make new game if it DNE
    let game = games.find((current, index) =>
    {
        return current.game_id == game_id
    });

    if(game == undefined)
    {
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

// prepare solution data structure
/*
[
    {
        game_id: ...
        players: 
                [
                    {
                        id: ...,
                        plus_minus: ...
                    }
                ]
    }
]
*/
let solution = [];

games.forEach(element => {

    // add new game
    let game = solution.find((current, index) =>
    {
        return current.game_id == element.game_id;
    });

    if(game == undefined)
    {
        let newgame =   { 
                            game_id: element.game_id,
                            players: []
                        };

        solution.push(newgame);
        game = newgame;
    }

    // add player
    element.team1.forEach(element => {
        
        let players = game.players.find((current, index) =>
        {
            return current.player_id == element;
        });

        if(players == undefined)
            game.players.push(
                                {
                                    id: element,
                                    plus_minus: 0
                                }
                            );
    });

    element.team2.forEach(element => {
        
        let players = game.players.find((current, index) =>
        {
            return current.player_id == element;
        });

        if(players == undefined)
            game.players.push(
                                {
                                    id: element,
                                    plus_minus: 0
                                }
                            );
    });

});

// read in plays 
let plays_entries = fs.readFileSync("plays.csv", "utf8").split(/[(?:|)]/g);

let plays = [] // array for unique game-period objects

lineup_entries.forEach(element => {
    
    data = element.split(/[(?:,)]/g);
    let game_id = data[0];
    let period = data[1];
    let player = data[2];
    let team = data[3];