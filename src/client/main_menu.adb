with Buttons;
with SDL;
with Screens;
with Strings;
with Fonts;

package body Main_Menu is
    Host, Join, Solo, Help, Edit, Quit : Buttons.Button;

    procedure Update_Buttons;

    procedure Draw is begin
        Update_Buttons;
        Buttons.Draw (Host);
        Buttons.Draw (Join);
        Buttons.Draw (Solo);
        Buttons.Draw (Help);
        Buttons.Draw (Edit);
        Buttons.Draw (Quit);
    end Draw;

    procedure Finalize is begin
        Buttons.Free_Overlay (Host);
        Buttons.Free_Overlay (Join);
        Buttons.Free_Overlay (Solo);
        Buttons.Free_Overlay (Help);
        Buttons.Free_Overlay (Edit);
        Buttons.Free_Overlay (Quit);
    end Finalize;

    procedure Initialize is begin
        Host := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Host, True));
        Join := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Join, True));
        Solo := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Solo, True));
        Help := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Help, True));
        Edit := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Edit, True));
        Quit := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Quit, True));
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Buttons;
        if Buttons.Process_Event (Host, What) then
            return (To => Screens.HOSTING);
        elsif Buttons.Process_Event (Join, What) then
            return (To => Screens.LOBBY);
        elsif Buttons.Process_Event (Solo, What) then
            return (To => Screens.ALONE);
        elsif Buttons.Process_Event (Help, What) then
            return (To => Screens.HELP);
        elsif Buttons.Process_Event (Edit, What) then
            return (To => Screens.PICKING);
        elsif Buttons.Process_Event (Quit, What) then
            return (To => Screens.QUIT);
        end if;
        return (To => Screens.NONE);
    end Process_Event;

    procedure Update_Buttons is
        W32 : Integer := SDL.State.Window.Width / 32;
        H24 : Integer := SDL.State.Window.Height / 24;
    begin
        Buttons.Set_Area (Host, (
            Left => W32 * 2,
            Top => H24 * 12,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Join, (
            Left => W32 * 12,
            Top => H24 * 12,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Solo, (
            Left => W32 * 22,
            Top => H24 * 12,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Help, (
            Left => W32 * 2,
            Top => H24 * 18,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Edit, (
            Left => W32 * 12,
            Top => H24 * 18,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Quit, (
            Left => W32 * 22,
            Top => H24 * 18,
            Width => W32 * 8,
            Height => H24 * 4));
    end Update_Buttons;

end Main_Menu;
