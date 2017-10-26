with SDL;
with Screens;

package Hosting_Screen is
    procedure Initialize;
    procedure Finalize;
    procedure Draw;
    procedure Update;

    function Process_Event (What : in SDL.Event) return Screens.Transition;
end Hosting_Screen;
