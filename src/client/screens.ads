--  Maintains the state machine of screens
package Screens is
    type Screen is (
        QUIT, NONE, MAIN_MENU, LOBBY, WAITING, HOSTING, PICKING, PLAYING,
        EDITING);
    type Transition (To : Screen) is
        record
            case To is
                when WAITING => Host : String; --  TODO: make this 'match data'
                when others => null; -- TODO
            end case;
        end record;
end Screens;
