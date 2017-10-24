with SDL;
with Screens;

package Main_Menu is
    procedure Initialize;
    procedure Finalize;
    procedure Draw;

    function Process_Event (What : in SDL.Event) return Screens.Transition;
end Main_Menu;
