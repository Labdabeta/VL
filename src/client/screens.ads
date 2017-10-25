--  Maintains the state machine of screens
with Boards;
with Maps;

package Screens is
    type Screen is (
        QUIT, NONE, MAIN_MENU, LOBBY, WAITING, HOSTING, PICKING, ALONE, PLAYING,
        EDITING, HELP, NEW_MAP, GET_NAME);
    type Transition (To : Screen) is
        record
            case To is
                when WAITING =>
                    Host : String (1 .. 120);
                    Name : String (1 .. 120);
                    Map_Name : String (1 .. 120);
                when EDITING =>
                    Document : Maps.Map;
                when others => null;
            end case;
        end record;

    --  Calls appropriate updates for screens
    procedure Apply_Transition (
        What : in Transition;
        To : in out Screen);

end Screens;
