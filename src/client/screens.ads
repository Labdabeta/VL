--  Maintains the state machine of screens
with Boards;
with Maps;

package Screens is
    type Screen is (
        QUIT,  --  DONE
        NONE,  --  DONE
        MAIN_MENU,  -- DONE
        LOBBY, -- DONE
        WAITING,   -- DONE??
        HOSTING,   -- DONE
        PICKING,   -- DONE
        ALONE, -- STUBBED FOR NOW!
        PLAYING,
        EDITING,  -- DONE
        HELP,
        NEW_MAP);  -- DONE
    type Transition (To : Screen) is
        record
            case To is
                when WAITING =>
                    Host : String (1 .. 120);
                    Name : String (1 .. 120);
                    Map_Name : String (1 .. 120);
                    Max_Players : Natural;
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
