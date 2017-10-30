with SDL;
with Screens;

package Lobby_Screen is
    procedure Initialize;
    procedure Finalize;
    procedure Draw;
    procedure Update;

    function Process_Event (What : in SDL.Event) return Screens.Transition;
end Lobby_Screen;
