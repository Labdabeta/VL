package body Units is

    function To_String (
        This : in Occupant)
        return String is
    begin
        case This is
            when VAMPIRE => return "V";
            when LEPRECHAUN => return "L";
            when HUMAN => return "H";
            when FAIRY => return "F";
            when ZOMBIE => return "Z";
            when NONE => return " ";
            when UNKNOWN => return "?";
        end case;
    end To_String;

end Units;
