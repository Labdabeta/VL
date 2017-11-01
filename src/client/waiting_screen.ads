with SDL;
with Screens;

package Waiting_Screen is
    procedure Initialize;
    procedure Finalize;
    procedure Draw;
    procedure Update (
        Host : String;
        Name : String;
        Map_Name : String;
        Max_Players : Natural);

    function Process_Event (What : in SDL.Event) return Screens.Transition;
end Waiting_Screen;
