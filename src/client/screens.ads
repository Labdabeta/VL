--  Maintains the state machine of screens
package Screens is
    type Screen is (
        QUIT, NONE, MAIN_MENU, LOBBY, WAITING, HOSTING, PICKING, ALONE, PLAYING,
        EDITING, HELP);
    type Transition (To : Screen) is
        record
            case To is
                when WAITING =>
                    Host : String (1 .. 120);
                    Name : String (1 .. 120);
                    Map_Name : String (1 .. 120);
                when others => null;
            end case;
        end record;

    --  Calls appropriate updates for screens
    procedure Apply_Transition (
        What : in Transition;
        To : in out Screen);

end Screens;
