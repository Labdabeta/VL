with Boards; use Boards;
with Buttons;
with Fonts;
with Games;
with Maps;
with Paths;
with Screens;
with SDL;
with Strings;

package body Picking_Screen is
    Prev, Next, Back, Create, Edit : Buttons.Button;
    Title : SDL.Image;
    Current : Maps.Map;
    Board_Area, Title_Area : SDL.Rectangle;

    procedure Update_Layout;
    procedure Retitle;

    procedure Draw is begin
        Update_Layout;
        Buttons.Draw (Prev);
        Buttons.Draw (Next);
        Buttons.Draw (Back);
        Buttons.Draw (Create);
        Buttons.Draw (Edit);
        if Current.Contents /= null then
            Games.Draw_Board (Current.Contents.all, Board_Area);
            SDL.Draw_Image_Centered (Title, Title_Area);
        end if;
    end Draw;

    procedure Finalize is begin
        Maps.Free_Maps (Current);
        Buttons.Free_Overlay (Prev);
        Buttons.Free_Overlay (Next);
        Buttons.Free_Overlay (Back);
        Buttons.Free_Overlay (Create);
        Buttons.Free_Overlay (Edit);
        if not SDL.Is_Null (Title) then
            SDL.Free_Image (Title);
        end if;
    end Finalize;

    procedure Initialize is begin
        Current.Contents := null;
        Prev := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Prev, True));
        Next := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Next, True));
        Back := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Back, True));
        Create := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Create, True));
        Edit := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Edit, True));
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Layout;
        if Buttons.Process_Event (Prev, What) then
            Maps.Prev (Current);
            Retitle;
        elsif Buttons.Process_Event (Next, What) then
            Maps.Next (Current);
            Retitle;
        elsif Buttons.Process_Event (Back, What) then
            return (To => Screens.MAIN_MENU);
        elsif Buttons.Process_Event (Create, What) then
            return (To => Screens.NEW_MAP);
        elsif Buttons.Process_Event (Edit, What) then
            if Current.Contents /= null then
                return (To => Screens.EDITING,
                    Document => Current);
            end if;
        end if;

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
        Buttons.Set_Area (Back, (
            Left => W32 * 2,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Create, (
            Left => W32 * 12,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Edit, (
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
    end Update_Layout;
end Picking_Screen;
