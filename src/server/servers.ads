with Actions;
with Lobby;
with VL;

package Servers is
    task type Server (Player_Count : Positive; Game : access VL.VL_Game) is
        entry Initialize (The_Lobby : in Lobby.Lobby_Element);
        entry Commit (Which : in Actions.Action_Array);
        entry Uncommit;
        entry Kill;
    end Server;
end Servers;
