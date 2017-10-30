with Buttons;
with Fonts;
with Lobby; use Lobby;
with Text_Lists;
with Screens;
with Paths;
with SDL;
with Strings;

package body Lobby_Screen is
    List_Height : constant := 16;
    List_Width : constant := 37;
    More, Back, Refresh : Buttons.Button;
    Selection_List : Text_Lists.Text_List (List_Width, List_Height);
    Num_Screens, Current_Screen : Positive;
    Lobby_Elements : Lobby.Lobby_Element_Array_Ptr;

    procedure Update_Layout;
    procedure Relist;

    procedure Draw is begin
        Update_Layout;
        Buttons.Draw (More);
        Buttons.Draw (Back);
        Buttons.Draw (Refresh);
        Text_Lists.Draw (Selection_List);
    end Draw;

    procedure Finalize is begin
        Lobby.Free (Lobby_Elements);
        Buttons.Free_Overlay (More);
        Buttons.Free_Overlay (Back);
        Buttons.Free_Overlay (Refresh);
        Text_Lists.Free (Selection_List);
    end Finalize;

    procedure Initialize is begin
        More := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.More, True));
        Back := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Back, True));
        Refresh := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Refresh, True));
        Selection_List := Text_Lists.Create (List_Width, List_Height);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
        Selected_Item : Natural;
        Selected_Element : Positive;
    begin
        Update_Layout;
        Selected_Item := Text_Lists.Process_Event (Selection_List, What);
        if Buttons.Process_Event (More, What) then
            if Current_Screen < Num_Screens then
                Current_Screen := Current_Screen + 1;
            else
                Current_Screen := 1;
            end if;
            Relist;
        elsif Buttons.Process_Event (Back, What) then
            return (To => Screens.MAIN_MENU);
        elsif Buttons.Process_Event (Refresh, What) then
            Update;
        elsif Selected_Item > 0 then
            Selected_Element := (Current_Screen - 1) * 16 + Selected_Item;
            if Selected_Element > Lobby_Elements'Length then
                return (To => Screens.NONE);
            end if;
            return (To => Screens.WAITING,
                Host => (Lobby_Elements (Selected_Element).Address),
                Name => (Lobby_Elements (Selected_Element).Name),
                Map_Name => (Lobby_Elements (Selected_Element).Map_Name),
                Max_Players => (Lobby_Elements (Selected_Element).Max_Players));
        end if;

        return (To => Screens.NONE);
    end Process_Event;

    procedure Relist is
        Current : Positive := (Current_Screen - 1) * 16 + 1;
    begin
        for Index in Positive range 1 .. List_Height loop
            if Current <= Lobby_Elements'Length then
                Text_Lists.Set_Line (
                    Selection_List,
                    Index,
                    Lobby_Elements (Current).Name (1 .. 18) & " " &
                    Positive'Image (Lobby_Elements (Current).Max_Players) &
                    " " & Lobby_Elements (Current).Map_Name (1 .. 15));
            else
                Text_Lists.Set_Line (Selection_List, Index, "");
            end if;
            Current := Current + 1;
        end loop;
    end Relist;

    procedure Update is begin
        if Lobby_Elements /= null then
            Lobby.Free (Lobby_Elements);
        end if;
        declare
            Transfer : Lobby.Lobby_Element_Array := Lobby.Query_Lobby;
        begin
            Lobby_Elements := new Lobby.Lobby_Element_Array (Transfer'Range);
            Lobby_Elements.all := Transfer;
            Num_Screens := (Transfer'Length / 16) + 1;
            Current_Screen := 1;
            Relist;
        end;
    end Update;

    procedure Update_Layout is
        W32 : Integer := SDL.State.Window.Width / 32;
        H24 : Integer := SDL.State.Window.Height / 24;
    begin
        Text_Lists.Set_Area (Selection_List, (
            Left => W32,
            Top => H24,
            Width => W32 * 30,
            Height => H24 * 17));
        Buttons.Set_Area (Back, (
            Left => W32 * 2,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Refresh, (
            Left => W32 * 12,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (More, (
            Left => W32 * 22,
            Top => H24 * 19,
            Width => W32 * 8,
            Height => H24 * 4));
    end Update_Layout;

end Lobby_Screen;
