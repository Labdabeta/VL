package body Players is

    function Get_Actions_From (
        This : in Player;
        Source : in Coordinates.Coordinate)
        return Actions.Action_Array is
        Temp_Ret : Actions.Action_Array (1 .. 0) := (
            others => ((1, 1), (1, 1), Actions.MOVE, Units.VAMPIRE, 1));
    begin

        return Temp_Ret;
        -- TODO

    end Get_Actions_From;

    function Get_Num_Actions (
        This : in Player)
        return Positive is
    begin

        return 1;
        -- TODO

    end Get_Num_Actions;

    function Get_Team (
        This : in Player)
        return Positive is

    begin

        return 1;
        -- TODO

    end Get_Team;

    function New_Player (
        Team : in Positive;
        Bases : in Coordinate_List;
        Unit_Coordinates : in Coordinate_List;
        Unit_Types : in Type_List;
        Productions : in Natural)
        return Player is
        Ret : Player := (
            Num_Bases => Bases'Length,
            Num_Units => Unit_Coordinates'Length,
            Bases => Bases,
            Unit_Coordinates => Unit_Coordinates,
            Unit_Types => Unit_Types,
            Team => Team,
            Productions => Productions);
    begin
        return Ret;
    end New_Player;

end Players;
