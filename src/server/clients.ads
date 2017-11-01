with Actions;
with Lobby;
with VL;

package Clients is
    task type Client (Game : access VL.VL_Game) is
        entry Initialize (The_Lobby : in Lobby.Lobby_Element);
        entry Commit (Which : in Actions.Action_Array);
        entry Uncommit;
        entry Kill;
    end Client;
end Clients;
