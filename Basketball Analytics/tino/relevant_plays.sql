/*
	Import plays, events and lineups as csvs and make tables
*/

/*
	Make temporary view of relevant events
    ie made shots, free throws, substitutions
*/
with relevant_events as
(
	select * from events
	where event_msg_type = 1
	or event_msg_type = 3
	or event_msg_type = 8
	order by event_msg_type
),

/*
	Make temporary view of relevant plays
	As join between plays and relevant_events
	Order plays by prompt recommendation
*/
relevant_plays as
(
	select 
	Game_id, Event_Num,plays.Event_Msg_Type, relevant_events.Event_Msg_Type_Description,
	Period, WC_Time, PC_Time,plays.Action_Type, relevant_events.Action_Type_Description,
	Option1, Option2, Option3, Team_id, Person1, Person2, Team_id_type
	from plays join relevant_events
	where plays.event_msg_type = relevant_events.event_msg_type 
	and plays.action_type = relevant_events.action_type
	order by game_id, Period , PC_Time desc, WC_Time , Event_Num
)

/*
	Export all relevant_plays as csv
*/