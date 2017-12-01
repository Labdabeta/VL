with SDL;
with Screens;

package Playing_Screen is
    procedure Initialize;
    procedure Finalize;
    procedure Draw;
    procedure Update; -- Reads from the VL_Manager
    function Process_Event (What : in SDL.Event) return Screens.Transition;
end Playing_Screen;

