package body Screens is
    procedure Apply_Transition (
        What : in Transition;
        To : in out Screen) is
    begin
        if What.To /= NONE then
            To := What.To;
        end if;
        --  TODO
    end Apply_Transition;
end Screens;
