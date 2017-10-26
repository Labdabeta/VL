with SDL;

package Buttons is
    type Button is private;

    function Create (
        Overlay : SDL.Image := SDL.Null_Image) return Button;

    procedure Set_Area (
        This : in out Button;
        Area : in SDL.Rectangle);

    procedure Free_Overlay (This : in out Button);

    function Process_Event (
        This : in out Button;
        What : in SDL.Event)
        return Boolean;

    procedure Draw (This : Button);

    --  Used to render a subsection of the overlay image
    procedure Draw_Area (
        This : Button;
        Overlay_Area : SDL.Rectangle;
        Inner_Sep : Natural);
private
    type Button is
        record
            Area : SDL.Rectangle;
            Overlay : SDL.Image;
            --  True if the current mouse-click started on this button
            Was_Pressed : Boolean;
        end record;
end Buttons;
