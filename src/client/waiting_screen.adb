with Boards; use Boards;
with Buttons;
with Fonts;
with Screens;
with Paths;
with SDL;
with Strings;
with VL;
with Games;

with Ada.Unchecked_Deallocation;

with Ada.Text_IO;

package body Waiting_Screen is
    Back : Buttons.Button;
    Map : Boards.Board_Ptr;
    Title : SDL.Image;
    Title_Area, Map_Area : SDL.Rectangle;

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
        if not SDL.Is_Null (Title) then
            SDL.Draw_Image_Centered (Title, Title_Area);
        end if;
        --  TODO: draw player states
    end Draw;

    procedure Finalize is begin
        VL.VL_Manager.Kill;
        Buttons.Free_Overlay (Back);
        if not SDL.Is_Null (Title) then
            SDL.Free_Image (Title);
        end if;
        if Map /= null then
            Boards.Free_Board (Map);
        end if;
    end Finalize;

    procedure Initialize is begin
        Back := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Back, True));
        Map := null;
        Title := SDL.Null_Image;
        Title_Area := (0, 0, 1, 1);
        Map_Area := (0, 0, 1, 1);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Layout;
        if Buttons.Process_Event (Back, What) then
            VL.VL_Manager.Detach;
            return (To => Screens.MAIN_MENU);
        end if;

        return (To => Screens.NONE);
    end Process_Event;

    procedure Refresh is
    begin
        Ada.Text_IO.Put_Line ("Refresh");
        Notifier.Clear;
    end Refresh;

    procedure Update (
        Host : String;
        Name : String;
        Map_Name : String;
        Max_Players : Natural) is
    begin
        if Host (Host'First) = ' ' then
            VL.VL_Manager.Host (Which => (Name, Map_Name, Host, Max_Players));
        else
            VL.VL_Manager.Join (Which => (Name, Map_Name, Host, Max_Players));
        end if;

        VL.VL_Manager.Set_Notifier (Notifier'Access);
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
        Map_Area := (
            Left => W32 * 3,
            Top => H24 * 2,
            Width => W32 * 26,
            Height => H24 * 16);
    end Update_Layout;
end Waiting_Screen;
