with SDL;
with Sprites;
with Fonts;

package body Text_Lists is
    function Get_Active_Element (This : Text_List) return Natural;
    function Get_Area (This : Text_List; Index : Positive) return SDL.Rectangle;

    function Create (
        Width : Positive;
        Height : Positive) return Text_List is
        Result : Text_List (Width => Width, Height => Height) := (
            Width => Width,
            Height => Height,
            Area => (0, 0, 1, 1),
            Overlays => (others => SDL.Null_Image),
            Was_Pressed => (others => False));
        Default_String : String (1 .. Width) := (others => '-');
    begin
        for Index in Result.Overlays'Range loop
            Result.Overlays (Index) := SDL.Render_Text (
                Fonts.Main_Font,
                Default_String,
                False);
        end loop;

        return Result;
    end Create;

    procedure Draw (This : Text_List) is
        Active_Element : Natural := Get_Active_Element (This);
        Correct_Clip : SDL.Rectangle;
        Correct_Area : SDL.Rectangle;
    begin
        for Index in This.Overlays'Range loop
            if Index = Active_Element then
                if SDL.State.Mouse.Buttons (SDL.LEFT) then
                    Correct_Clip := Sprites.Text_List_Pressed_Clip;
                else
                    Correct_Clip := Sprites.Text_List_Hover_Clip;
                end if;
            else
                Correct_Clip := Sprites.Text_List_Normal_Clip;
            end if;

            Correct_Area := Get_Area (This, Index);
            SDL.Draw_Image (Sprites.Text_List_Sprites, Correct_Area,
                Correct_Clip);
            SDL.Draw_Image_Centered (This.Overlays (Index), Correct_Area);
        end loop;
    end Draw;

    procedure Free (This : in out Text_List) is begin
        for Index in This.Overlays'Range loop
            SDL.Free_Image (This.Overlays (Index));
        end loop;
    end Free;

    function Get_Active_Element (This : Text_List) return Natural is
        Local_Area : SDL.Rectangle;
    begin
        Local_Area.Left := This.Area.Left;
        Local_Area.Width := This.Area.Width;
        Local_Area.Height := This.Area.Height / This.Height;
        Local_Area.Top := This.Area.Top;
        for Index in This.Overlays'Range loop
            if SDL.Within (Local_Area, SDL.State.Mouse.Where) then
                return Index;
            end if;

            Local_Area.Top := Local_Area.Top + Local_Area.Height;
        end loop;
        return 0;
    end Get_Active_Element;

    function Get_Area (This : Text_List; Index : Positive) return SDL.Rectangle
    is begin
        return (This.Area.Left,
            This.Area.Top + (This.Area.Height / This.Height) * (Index - 1),
            This.Area.Width,
            This.Area.Height / This.Height);
    end Get_Area;

    function Process_Event (
        This : in out Text_List;
        What : in SDL.Event)
        return Natural is
        Active_Element : Natural := Get_Active_Element (This);
    begin
        case What.Kind is
            when SDL.MOUSE_DOWN_EVENT =>
                for Index in This.Was_Pressed'Range loop
                    if Index = Active_Element then
                        This.Was_Pressed (Index) := True;
                    else
                        This.Was_Pressed (Index) := False;
                    end if;
                end loop;
            when SDL.MOUSE_UP_EVENT =>
                if Active_Element > 0 then
                    if This.Was_Pressed (Active_Element) then
                        return Active_Element;
                    end if;
                end if;
            when others => null;
        end case;

        return 0;
    end Process_Event;

    procedure Set_Area (
        This : in out Text_List;
        Area : in SDL.Rectangle) is
    begin
        This.Area := Area;
    end Set_Area;

    procedure Set_Line (
        This : in out Text_List;
        Which : in Positive;
        Data : in String) is
        Default_String : String (1 .. This.Width) := (others => '-');
    begin
        SDL.Free_Image (This.Overlays (Which));
        if Data'Length > 0 then
            This.Overlays (Which) := SDL.Render_Text (
                Fonts.Main_Font,
                Data,
                False);
        else
            This.Overlays (Which) := SDL.Render_Text (
                Fonts.Main_Font,
                Default_String,
                False);
        end if;
    end Set_Line;
end Text_Lists;
