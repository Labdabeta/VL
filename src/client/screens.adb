with Picking_Screen;
with New_Map_Screen;
with Editing_Screen;

package body Screens is
    procedure Apply_Transition (
        What : in Transition;
        To : in out Screen) is
    begin
        if What.To /= NONE then
            To := What.To;
        end if;

        case What.To is
            when PICKING => Picking_Screen.Update;
            when NEW_MAP => New_Map_Screen.Update;
            when EDITING => Editing_Screen.Update (What.Document);
            when others => null;
        end case;
    end Apply_Transition;
end Screens;
