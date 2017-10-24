with SDL;
with Sprites;

package body Buttons is
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
            SDL.Draw_Image (This.Overlay, (
                Left => This.Area.Left +
                    (This.Area.Width / 2) -
                    (This.Overlay.Width / 2),
                Top => This.Area.Top +
                    (This.Area.Height / 2) -
                    (This.Overlay.Height / 2),
                Width => This.Overlay.Width,
                Height => This.Overlay.Height));
        end if;
    end Draw;

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
end Buttons;
