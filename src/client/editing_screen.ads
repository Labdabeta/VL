with SDL;
with Screens;
with Maps;

package Editing_Screen is
    procedure Initialize;
    procedure Finalize;
    procedure Draw;
    procedure Update (Document : Maps.Map);

    function Process_Event (What : in SDL.Event) return Screens.Transition;
end Editing_Screen;
