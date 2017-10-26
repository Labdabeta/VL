with SDL; use SDL;
with Screens;
with Sprites;
with Fonts;

with Main_Menu;
with Picking_Screen;
with New_Map_Screen;
with Editing_Screen;
with Hosting_Screen;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    Current : Screens.Screen := Screens.MAIN_MENU;
    Next : Time;
    One_Frame : Time_Span := Microseconds (16667);
begin
    if not Initialize ("Vampire Leprechauns", 1024, 768) then
        return;
    end if;

    Sprites.Initialize;
    Fonts.Initialize;
    Main_Menu.Initialize;
    Picking_Screen.Initialize;
    New_Map_Screen.Initialize;
    Editing_Screen.Initialize;
    Hosting_Screen.Initialize;

    Frame_Loop : loop
        Next := Clock + One_Frame;
        Event_Loop : loop
            declare
                E : Event := Step;
            begin
                if E.Kind = QUIT_EVENT then
                    exit Frame_Loop;
                elsif E.Kind = NO_EVENT then
                    exit Event_Loop;
                end if;
                case Current is
                    when Screens.MAIN_MENU =>
                        Screens.Apply_Transition (
                            Main_Menu.Process_Event (E),
                            Current);
                    when Screens.PICKING =>
                        Screens.Apply_Transition (
                            Picking_Screen.Process_Event (E),
                            Current);
                    when Screens.NEW_MAP =>
                        Screens.Apply_Transition (
                            New_Map_Screen.Process_Event (E),
                            Current);
                    when Screens.EDITING =>
                        Screens.Apply_Transition (
                            Editing_Screen.Process_Event (E),
                            Current);
                    when Screens.HOSTING =>
                        Screens.Apply_Transition (
                            Hosting_Screen.Process_Event (E),
                            Current);
                    when others => exit Frame_Loop;
                end case;
            end;
        end loop Event_Loop;

        Begin_Draw;
        SDL.Draw_Image (
            Sprites.Background_Sprite,
            (0, 0, State.Window.Width, State.Window.Height));
        case Current is
            when Screens.MAIN_MENU => Main_Menu.Draw;
            when Screens.PICKING => Picking_Screen.Draw;
            when Screens.NEW_MAP => New_Map_Screen.Draw;
            when Screens.EDITING => Editing_Screen.Draw;
            when Screens.HOSTING => Hosting_Screen.Draw;
            when others => exit Frame_Loop;
        end case;
        End_Draw;

        if Next > Clock then
            delay To_Duration (abs (Next - Clock));
        end if;
    end loop Frame_Loop;

    Hosting_Screen.Finalize;
    Editing_Screen.Finalize;
    New_Map_Screen.Finalize;
    Picking_Screen.Finalize;
    Main_Menu.Finalize;
    Fonts.Finalize;
    Sprites.Finalize;
    Finalize;
end Main;
