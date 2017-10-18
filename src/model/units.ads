package Units is
    type Occupant is (VAMPIRE, LEPRECHAUN, HUMAN, FAIRY, ZOMBIE, NONE, UNKNOWN);
    for Occupant use (
        VAMPIRE     => 0,
        LEPRECHAUN  => 1,
        HUMAN       => 2,
        FAIRY       => 3,
        ZOMBIE      => 4,
        NONE        => 5,
        UNKNOWN     => 6);
    subtype Unit is Occupant range VAMPIRE .. ZOMBIE;

    Num_Unit_Types : constant := 5;

    Vision : constant array (Occupant) of Natural := (
        VAMPIRE => 4,
        LEPRECHAUN => 3,
        HUMAN => 3,
        FAIRY => 3,
        ZOMBIE => 2,
        NONE => 0,
        UNKNOWN => 0);

    Max_Vision : constant := 4;

    function To_String (
        This : in Occupant)
        return String;
end Units;
