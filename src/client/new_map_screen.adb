with SDL;
with Buttons;
with Fonts;
with Text_Boxes;
with Strings;
with Maps;

package body New_Map_Screen is
    Back, Create, Add_Width, Add_Height, Sub_Width, Sub_Height : Buttons.Button;
    Name : Text_Boxes.Text_Box;
    Width, Height, X : SDL.Image;
    W, H : Positive;
    Width_Area, Height_Area, X_Area : SDL.Rectangle;

    procedure Update_Layout;

    procedure Draw is begin
        Update_Layout;
        Buttons.Draw (Back);
        Buttons.Draw (Create);
        Buttons.Draw (Add_Width);
        Buttons.Draw (Add_Height);
        Buttons.Draw (Sub_Width);
        Buttons.Draw (Sub_Height);
        Text_Boxes.Draw (Name);
        SDL.Draw_Image (Width, (
            Left => Width_Area.Left +
                (Width_Area.Width / 2) -
                (Width.Width / 2),
            Top => Width_Area.Top +
                (Width_Area.Width / 2) -
                (Width.Height / 2),
            Width => Width.Width,
            Height => Width.Height));
        SDL.Draw_Image (Height, (
            Left => Height_Area.Left +
                (Height_Area.Width / 2) -
                (Height.Width / 2),
            Top => Height_Area.Top +
                (Height_Area.Width / 2) -
                (Height.Height / 2),
            Width => Height.Width,
            Height => Height.Height));
        SDL.Draw_Image (X, (
            Left => X_Area.Left +
                (X_Area.Width / 2) -
                (X.Width / 2),
            Top => X_Area.Top +
                (X_Area.Width / 2) -
                (X.Height / 2),
            Width => X.Width,
            Height => X.Height));
    end Draw;

    procedure Finalize is begin
        if not SDL.Is_Null (Name.Current) then
            SDL.Free_Image (Name.Current);
        end if;
        SDL.Free_Image (Back.Overlay);
        SDL.Free_Image (Create.Overlay);
        SDL.Free_Image (Add_Width.Overlay);
        SDL.Free_Image (Add_Height.Overlay);
        SDL.Free_Image (Sub_Width.Overlay);
        SDL.Free_Image (Sub_Height.Overlay);
        SDL.Free_Image (Width);
        SDL.Free_Image (Height);
        SDL.Free_Image (X);
    end Finalize;

    procedure Initialize is begin
        W := 1;
        H := 1;
        Back.Overlay := SDL.Render_Text (Fonts.Main_Font, Strings.Back, True);
        Create.Overlay := SDL.Render_Text (Fonts.Main_Font,
            Strings.Create_Map, True);
        Add_Width.Overlay := SDL.Render_Text (Fonts.Main_Font,
            Strings.Add_Width, True);
        Add_Height.Overlay := SDL.Render_Text (Fonts.Main_Font,
            Strings.Add_Height, True);
        Sub_Width.Overlay := SDL.Render_Text (Fonts.Main_Font,
            Strings.Sub_Width, True);
        Sub_Height.Overlay := SDL.Render_Text (Fonts.Main_Font,
            Strings.Sub_Height, True);
        Width := SDL.Render_Text (Fonts.Main_Font, Positive'Image (W), False);
        Height := SDL.Render_Text (Fonts.Main_Font, Positive'Image (H), False);
        X := SDL.Render_Text (Fonts.Main_Font, Strings.X, True);
        Name := Text_Boxes.Create_Text_Box (Strings.Name_Hint, True);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Layout;
        if Buttons.Process_Event (Back, What) then
            return (To => Screens.PICKING);
        elsif Buttons.Process_Event (Create, What) then
            if Name.Length > 0 then
                return (To => Screens.EDITING,
                    Document => Maps.Create_Map (
                        Name.Contents (1 .. Name.Length), W, H));
            end if;
        elsif Buttons.Process_Event (Add_Width, What) then
            W := W + 1;
            SDL.Free_Image (Width);
            Width :=
                SDL.Render_Text (Fonts.Main_Font, Positive'Image (W), False);
        elsif Buttons.Process_Event (Add_Height, What) then
            H := H + 1;
            SDL.Free_Image (Height);
            Height :=
                SDL.Render_Text (Fonts.Main_Font, Positive'Image (H), False);
        elsif Buttons.Process_Event (Sub_Width, What) then
            if W > 1 then
                W := W - 1;
            end if;
            SDL.Free_Image (Width);
            Width :=
                SDL.Render_Text (Fonts.Main_Font, Positive'Image (W), False);
        elsif Buttons.Process_Event (Sub_Height, What) then
            if H > 1 then
                H := H - 1;
            end if;
            SDL.Free_Image (Height);
            Height :=
                SDL.Render_Text (Fonts.Main_Font, Positive'Image (H), False);
        end if;
        Text_Boxes.Process_Event (Name, What);
        return (To => Screens.NONE);
    end Process_Event;

    procedure Update is begin
        W := 1;
        H := 1;
        SDL.Free_Image (Height);
        Height := SDL.Render_Text (Fonts.Main_Font, Positive'Image (H), False);
        Width := SDL.Render_Text (Fonts.Main_Font, Positive'Image (W), False);
        Text_Boxes.Reset (Name);
    end Update;

    procedure Update_Layout is
        W32 : Integer := SDL.State.Window.Width / 32;
        H24 : Integer := SDL.State.Window.Height / 24;
    begin
        Back.Area := (
            Left => W32 * 2,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4);
        Create.Area := (
            Left => W32 * 22,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4);
        Add_Width.Area := (
            Left => W32 * 11,
            Top => H24 * 8,
            Width => W32 * 4,
            Height => H24 * 4);
        Add_Height.Area := (
            Left => W32 * 17,
            Top => H24 * 8,
            Width => W32 * 4,
            Height => H24 * 4);
        Sub_Width.Area := (
            Left => W32 * 11,
            Top => H24 * 15,
            Width => W32 * 4,
            Height => H24 * 4);
        Sub_Height.Area := (
            Left => W32 * 17,
            Top => H24 * 15,
            Width => W32 * 4,
            Height => H24 * 4);
        Name.Area := (
            Left => W32 * 2,
            Top => H24 * 5,
            Width => W32 * 28,
            Height => H24 * 2);
        Width_Area := (
            Left => W32 * 11,
            Top => H24 * 12,
            Width => W32 * 4,
            Height => H24 * 3);
        Height_Area := (
            Left => W32 * 17,
            Top => H24 * 12,
            Width => W32 * 4,
            Height => H24 * 3);
        X_Area := (
            Left => W32 * 15,
            Top => H24 * 13,
            Width => W32 * 2,
            Height => H24 * 3);
    end Update_Layout;

end New_Map_Screen;
