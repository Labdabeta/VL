with SDL; use SDL;
with Screens;
with Sprites;
with Fonts;
with Main_Menu;

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
                    when others => exit Frame_Loop;
                end case;
            end;
        end loop Event_Loop;

        Begin_Draw;
        case Current is
            when Screens.MAIN_MENU =>
                Main_Menu.Draw;
            when others => exit Frame_Loop;
        end case;
        End_Draw;

        if Next > Clock then
            delay To_Duration (abs (Next - Clock));
        end if;
    end loop Frame_Loop;

    Main_Menu.Finalize;
    Fonts.Finalize;
    Sprites.Finalize;
    Finalize;
end Main;
