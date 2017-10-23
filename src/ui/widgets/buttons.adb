with SDL;

package body Buttons is
    procedure Draw (This : Button) is
        Correct_Image : SDL.Image;
    begin
        if SDL.Within (This.Area, SDL.State.Mouse.Where) then
            if SDL.State.Mouse.Buttons (SDL.LEFT) then
                Correct_Image := This.Pressed;
            else
                Correct_Image := This.Hover;
            end if;
        else
            Correct_Image := This.Normal;
        end if;

        SDL.Draw_Image (Correct_Image, This.Area);
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
