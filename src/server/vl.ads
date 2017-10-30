with Lobby;
with Boards;
with Actions;
with Servers;

package VL is
    task type VL_Game is
        entry Host (Which : in Lobby.Lobby_Element);
        entry Join (Which : in Lobby.Lobby_Element);

        entry Get_Dimensions (
            Width : out Positive;
            Height : out Positive;
            Player_Count : out Positive);

        entry Commit (Which : in Actions.Action_Array);
        entry Uncommit;

        entry Check (Changed : out Boolean);

        entry Query (
            Board : out Boards.Board;
            Statuses : out Servers.Player_Status_Array);

        entry Quit;
        entry Kill;
    end VL_Game;
end VL;
