with Boards; use Boards;
with Buttons;
with Fonts;
with Screens;
with Paths;
with SDL;
with Strings;
with VL;
with Games;

package body Playing_Screen is
    Back, Submit : Buttons.Button;
    Map : Boards.Board_Ptr;
    Map_Area : SDL.Rectangle;

    Notifier : aliased VL.VL_Notifier;

    procedure Update_Layout;
    procedure Refresh;

    procedure Draw is
        Has_Update : Boolean;
    begin
        Notifier.Query (Has_Update);
        if Has_Update then
            Refresh;
        end if;
        Update_Layout;
        Buttons.Draw (Back);
        if Map /= null then
            Games.Draw_Board (Map.all, Map_Area);
        end if;
        -- TODO: draw player states/actions/etc
    end Draw;

    procedure Finalize is begin
        Buttons.Free_Overlay (Back);
        Buttons.Free_Overlay (Submit);
        if Map /= null then
            Boards.Free_Board (Map);
        end if;
    end Finalize;

    procedure Initialize is begin
        Back := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Back, True));
        Map := null;
        Map_Area := (0, 0, 1, 1);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Layout;
        if Buttons.Process_Event (Back, What) then
            VL.VL_Manager.Detach;
            return (To => Screens.MAIN_MENU);
        end if;

        -- TODO: submit/click

        return (To => Screens.NONE);
    end Process_Event;

    procedure Refresh is
    begin
        null;
    end Refresh;

    procedure Update is
    begin
        VL.VL_Manager.Set_Notifier (Notifier'Access);
    end Update;

    procedure Update_Layout is
        W32 : Integer := SDL.State.Window.Width / 32;
        H24 : Integer := SDL.State.Window.Height / 24;
    begin
        Buttons.Set_Area (Submit, (
            Left => W32 * 24,
            Top => H24 * 16,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Back, (
            Left => W32 * 24,
            Top => H24 * 20,
            Width => W32 * 8,
            Height => H24 * 4));
        Map_Area := (
            Left => 0,
            Top => 0,
            Width => W32 * 24,
            Height => H24 * 24);
    end Update_Layout;
end Playing_Screen;
