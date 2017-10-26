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
        SDL.Draw_Image_Centered (Width, Width_Area);
        SDL.Draw_Image_Centered (Height, Height_Area);
        SDL.Draw_Image_Centered (X, X_Area);
    end Draw;

    procedure Finalize is begin
        Text_Boxes.Free (Name);
        Buttons.Free_Overlay (Back);
        Buttons.Free_Overlay (Create);
        Buttons.Free_Overlay (Add_Width);
        Buttons.Free_Overlay (Add_Height);
        Buttons.Free_Overlay (Sub_Width);
        Buttons.Free_Overlay (Sub_Height);
        SDL.Free_Image (Width);
        SDL.Free_Image (Height);
        SDL.Free_Image (X);
    end Finalize;

    procedure Initialize is begin
        W := 1;
        H := 1;
        Back := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Back, True));
        Create := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Create_Map, True));
        Add_Width := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Add_Width, True));
        Add_Height := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Add_Height, True));
        Sub_Width := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Sub_Width, True));
        Sub_Height := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Sub_Height, True));
        Width := SDL.Render_Text (Fonts.Main_Font, Positive'Image (W), False);
        Height := SDL.Render_Text (Fonts.Main_Font, Positive'Image (H), False);
        X := SDL.Render_Text (Fonts.Main_Font, Strings.X, True);
        Name := Text_Boxes.Create (Strings.Name_Hint, 34, True);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Layout;
        if Buttons.Process_Event (Back, What) then
            return (To => Screens.PICKING);
        elsif Buttons.Process_Event (Create, What) then
            if Text_Boxes.Get_Content (Name)'Length > 0 then
                return (To => Screens.EDITING,
                    Document => Maps.Create_Map (
                        Text_Boxes.Get_Content (Name), W, H));
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
        Buttons.Set_Area (Back, (
            Left => W32 * 2,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Create, (
            Left => W32 * 22,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Add_Width, (
            Left => W32 * 11,
            Top => H24 * 8,
            Width => W32 * 4,
            Height => H24 * 4));
        Buttons.Set_Area (Add_Height, (
            Left => W32 * 17,
            Top => H24 * 8,
            Width => W32 * 4,
            Height => H24 * 4));
        Buttons.Set_Area (Sub_Width, (
            Left => W32 * 11,
            Top => H24 * 15,
            Width => W32 * 4,
            Height => H24 * 4));
        Buttons.Set_Area (Sub_Height, (
            Left => W32 * 17,
            Top => H24 * 15,
            Width => W32 * 4,
            Height => H24 * 4));
        Text_Boxes.Set_Area (Name, (
            Left => W32 * 2,
            Top => H24 * 5,
            Width => W32 * 28,
            Height => H24 * 2));
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
