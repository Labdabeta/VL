with SDL;
with Sprites;
with Colours;
with Fonts;

package body Text_Boxes is
    HOffset : constant := 10;

    procedure Update (This : in out Text_Box);

    function Create_Text_Box (
        Hint : String;
        Enabled : Boolean) return Text_Box is
        Result : Text_Box;
    begin
        Result.Contents := (others => ' ');
        Result.Length := 0;
        Result.Is_Active := Enabled;
        if Hint'Length > 120 then
            Result.Hint := Hint (Hint'First .. Hint'First + 119);
        else
            Result.Hint (1 .. Hint'Length) := Hint;
        end if;
        Result.Hint_Length := Hint'Length;
        Result.Current := SDL.Null_Image;
        Update (Result);
        return Result;
    end Create_Text_Box;

    procedure Draw (This : Text_Box) is
    begin
        if This.Is_Active then
            SDL.Draw_Image (Sprites.Text_Box_Sprites, This.Area,
                Sprites.Text_Box_Enabled_Clip);
        else
            SDL.Draw_Image (Sprites.Text_Box_Sprites, This.Area,
                Sprites.Text_Box_Disabled_Clip);
        end if;

        if not SDL.Is_Null (This.Current) then
            SDL.Draw_Image (
                This.Current, (
                    Left => This.Area.Left + HOffset,
                    Top => This.Area.Top +
                        (This.Area.Height / 2) -
                        (This.Current.Height / 2),
                    Width => This.Current.Width,
                    Height => This.Current.Height));
        end if;
    end Draw;

    procedure Process_Event (
        This : in out Text_Box;
        What : in SDL.Event) is
        procedure Append (What : Character) is begin
            This.Length := This.Length + 1;
            This.Contents (This.Length) := What;
            Update (This);
        end Append;

        Shifted : String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ)!@#$%^&*(" & ('"') &
            "?<??????+??0123456789.????{}??_???>?:? ??";
        Normal : String := "abcdefghijklmnopqrstuvwxyz0123456789'" &
            "?,??????=??0123456789.????[]??-???.?;/ ??";
    begin
        case What.Kind is
            when SDL.MOUSE_DOWN_EVENT =>
                if SDL.Within (This.Area, SDL.State.Mouse.Where) then
                    This.Is_Active := True;
                else
                    This.Is_Active := False;
                end if;
            when SDL.KEY_DOWN_EVENT =>
                if This.Is_Active then
                    case What.Key is
                        when SDL.KEY_A .. SDL.KEY_APOSTROPHE
                           | SDL.KEY_COMMA
                           | SDL.KEY_EQUALS
                           | SDL.KEY_PAD0 .. SDL.KEY_PAD_DOT
                           | SDL.KEY_LEFT_BRACKET
                           | SDL.KEY_RIGHT_BRACKET
                           | SDL.KEY_MINUS
                           | SDL.KEY_DOT
                           | SDL.KEY_SEMICOLON .. SDL.KEY_SPACE =>
                            if SDL.State.Keyboard (SDL.KEY_LEFT_SHIFT) or
                                SDL.State.Keyboard (SDL.KEY_RIGHT_SHIFT)
                            then
                                Append (
                                    Shifted (SDL.Key_Type'Pos (What.Key) + 1));
                            else
                                Append (
                                    Normal (SDL.Key_Type'Pos (What.Key) + 1));
                            end if;
                        when SDL.KEY_BACKSPACE =>
                            if This.Length > 0 then
                                This.Length := This.Length - 1;
                                Update (This);
                            end if;
                        when SDL.KEY_ESCAPE =>
                            This.Is_Active := False;
                        when others => null;
                    end case;
                end if;
            when others => null;
        end case;
    end Process_Event;

    procedure Reset (This : in out Text_Box) is
    begin
        This.Length := 0;
        Update (This);
    end Reset;

    procedure Update (This : in out Text_Box) is
    begin
        if not SDL.Is_Null (This.Current) then
            SDL.Free_Image (This.Current);
        end if;

        if This.Length = 0 then
            if This.Hint_Length > 0 then
                This.Current := SDL.Render_Text (
                    Fonts.Main_Font,
                    This.Hint (1 .. This.Hint_Length),
                    False,
                    Colours.Grey);
            end if;
        else
            This.Current := SDL.Render_Text (
                Fonts.Main_Font,
                This.Contents (1 .. This.Length),
                False);
        end if;
    end Update;
end Text_Boxes;
