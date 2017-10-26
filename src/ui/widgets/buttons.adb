with SDL;
with Sprites;

package body Buttons is
    function Create (
        Overlay : SDL.Image := SDL.Null_Image) return Button is
        Result : Button := (
            Area => (0, 0, 1, 1),
            Overlay => Overlay,
            Was_Pressed => False);
    begin
        return Result;
    end Create;

    procedure Draw (This : Button) is
        Correct_Clip : SDL.Rectangle;
    begin
        if SDL.Within (This.Area, SDL.State.Mouse.Where) then
            if SDL.State.Mouse.Buttons (SDL.LEFT) then
                Correct_Clip := Sprites.Button_Pressed_Clip;
            else
                Correct_Clip := Sprites.Button_Hover_Clip;
            end if;
        else
            Correct_Clip := Sprites.Button_Normal_Clip;
        end if;

        SDL.Draw_Image (Sprites.Button_Sprites, This.Area, Correct_Clip);
        if not SDL.Is_Null (This.Overlay) then
            SDL.Draw_Image_Centered (This.Overlay, This.Area);
        end if;
    end Draw;

    procedure Draw_Area (
        This : Button;
        Overlay_Area : SDL.Rectangle;
        Inner_Sep : Natural) is
        Correct_Clip : SDL.Rectangle;
    begin
        if SDL.Within (This.Area, SDL.State.Mouse.Where) then
            if SDL.State.Mouse.Buttons (SDL.LEFT) then
                Correct_Clip := Sprites.Button_Pressed_Clip;
            else
                Correct_Clip := Sprites.Button_Hover_Clip;
            end if;
        else
            Correct_Clip := Sprites.Button_Normal_Clip;
        end if;

        SDL.Draw_Image (Sprites.Button_Sprites, This.Area, Correct_Clip);
        if not SDL.Is_Null (This.Overlay) then
            SDL.Draw_Image (This.Overlay, (
                This.Area.Left + Inner_Sep,
                This.Area.Top + Inner_Sep,
                This.Area.Width - (2 * Inner_Sep),
                This.Area.Height - (2 * Inner_Sep)),
                Overlay_Area);
        end if;
    end Draw_Area;

    procedure Free_Overlay (This : in out Button) is
    begin
        SDL.Free_Image (This.Overlay);
    end Free_Overlay;

    function Process_Event (
        This : in out Button;
        What : in SDL.Event)
        return Boolean is
    begin
        case What.Kind is
            when SDL.MOUSE_DOWN_EVENT =>
                if SDL.Within (This.Area, SDL.State.Mouse.Where) then
                    This.Was_Pressed := True;
                else
                    This.Was_Pressed := False;
                end if;
            when SDL.MOUSE_UP_EVENT =>
                if SDL.Within (This.Area, SDL.State.Mouse.Where) and
                    This.Was_Pressed
                then
                    return True;
                end if;
            when others => null;
        end case;

        return False;
    end Process_Event;

    procedure Set_Area (
        This : in out Button;
        Area : in SDL.Rectangle) is
    begin
        This.Area := Area;
    end Set_Area;
end Buttons;
