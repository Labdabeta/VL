package body Units is

    function To_String (
        This : in Occupant)
        return String is
    begin
        case This is
            when VAMPIRE => return ASCII.ESC & "[31m";
            when LEPRECHAUN => return ASCII.ESC & "[92m";
            when HUMAN => return ASCII.ESC & "[32m";
            when FAIRY => return ASCII.ESC & "[35m";
            when ZOMBIE => return ASCII.ESC & "[90m";
            when NONE => return ASCII.ESC & "[97m";
            when UNKNOWN => return ASCII.ESC & "[30m";
        end case;
    end To_String;

end Units;
