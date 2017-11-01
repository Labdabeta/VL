with Boards; use Boards;
with Buttons;
with Fonts;
with Games;
with Maps;
with Paths;
with Screens;
with SDL;
with Strings;
with Text_Boxes;

package body Hosting_Screen is
    Prev, Next, Cancel, Host : Buttons.Button;
    Name : Text_Boxes.Text_Box;
    Title : SDL.Image;
    Current : Maps.Map;
    Board_Area, Title_Area : SDL.Rectangle;

    procedure Update_Layout;
    procedure Retitle;

    procedure Draw is begin
        Update_Layout;
        Buttons.Draw (Prev);
        Buttons.Draw (Next);
        Buttons.Draw (Cancel);
        Buttons.Draw (Host);
        Text_Boxes.Draw (Name);
        if Current.Contents /= null then
            Games.Draw_Board (Current.Contents.all, Board_Area);
            SDL.Draw_Image_Centered (Title, Title_Area);
        end if;
    end Draw;

    procedure Finalize is begin
        Maps.Free_Maps (Current);
        Buttons.Free_Overlay (Prev);
        Buttons.Free_Overlay (Next);
        Buttons.Free_Overlay (Cancel);
        Buttons.Free_Overlay (Host);
        if not SDL.Is_Null (Title) then
            SDL.Free_Image (Title);
        end if;
        Text_Boxes.Free (Name);
    end Finalize;

    procedure Initialize is begin
        Current.Contents := null;
        Prev := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Prev, True));
        Next := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Next, True));
        Cancel := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Cancel, True));
        Host := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Host, True));
        Name := Text_Boxes.Create (Strings.Room_Name, 12, True);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
        function Pad_To (Text : in String; Length : in Natural) return String is
            Result : String (1 .. Length) := (others => ' ');
            Next_Spot : Positive := 1;
        begin
            for Index in Text'Range loop
                Result (Next_Spot) := Text (Index);
                Next_Spot := Next_Spot + 1;
            end loop;

            return Result;
        end Pad_To;
    begin
        Update_Layout;
        if Buttons.Process_Event (Prev, What) then
            Maps.Prev (Current);
            Retitle;
        elsif Buttons.Process_Event (Next, What) then
            Maps.Next (Current);
            Retitle;
        elsif Buttons.Process_Event (Cancel, What) then
            return (To => Screens.MAIN_MENU);
        elsif Buttons.Process_Event (Host, What) then
            if Current.Contents /= null then
                return (To => Screens.WAITING,
                    Host => (others => ' '), -- special string indicating local
                    Name => Pad_To (Text_Boxes.Get_Content (Name), 120),
                    Map_Name => Pad_To (
                        Current.Name ( -- TODO: clean this up!
                            Current.Name'First .. Current.Name'First +
                            Current.Name_Length - 1), 120),
                    Max_Players =>
                        Boards.Get_Players (Current.Contents.all)'Length);
            end if;
        end if;
        Text_Boxes.Process_Event (Name, What);

        return (To => Screens.NONE);
    end Process_Event;

    procedure Retitle is begin
        if not SDL.Is_Null (Title) then
            SDL.Free_Image (Title);
        end if;

        if Current.Contents /= null then
            Title := SDL.Render_Text (Fonts.Main_Font,
                Current.Name (1 .. Current.Name_Length), False);
        end if;
    end Retitle;

    procedure Update is begin
        if Current.Contents /= null then
            Maps.Free_Maps (Current);
        end if;
        Current := Maps.Load_Maps;
        Text_Boxes.Reset (Name);
        Retitle;
    end Update;

    procedure Update_Layout is
        W32 : Integer := SDL.State.Window.Width / 32;
        H24 : Integer := SDL.State.Window.Height / 24;
    begin
        Buttons.Set_Area (Prev, (
            Left => W32 * 2,
            Top => H24 * 2,
            Width => W32 * 4,
            Height => H24 * 16));
        Buttons.Set_Area (Next, (
            Left => W32 * 26,
            Top => H24 * 2,
            Width => W32 * 4,
            Height => H24 * 16));
        Buttons.Set_Area (Cancel, (
            Left => W32 * 2,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Host, (
            Left => W32 * 22,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Board_Area := (
            Left => W32 * 8,
            Top => H24 * 2,
            Width => W32 * 16,
            Height => H24 * 16);
        Title_Area := (
            Left => 0,
            Top => 0,
            Width => W32 * 32,
            Height => H24 * 2);
        Text_Boxes.Set_Area (Name, (
            Left => W32 * 11,
            Top => H24 * 20,
            Width => W32 * 10,
            Height => H24 * 2));
    end Update_Layout;
end Hosting_Screen;
