with Actions;
with Coordinates;
with Units;

package Players is
    type Player (Num_Bases : Positive; Num_Units : Natural) is private;
    type Player_Handle is not null access Player;

    type Coordinate_List is array (Positive range <>) of Coordinates.Coordinate;
    type Type_List is array (Positive range <>) of Units.Unit;

    function New_Player (
        Team : in Positive;
        Bases : in Coordinate_List;
        Unit_Coordinates : in Coordinate_List;
        Unit_Types : in Type_List;
        Productions : Natural)
        return Player;

    function Get_Actions_From (
        This : in Player;
        Source : in Coordinates.Coordinate)
        return Actions.Action_Array;

    function Get_Team (
        This : in Player)
        return Positive;

    function Get_Num_Actions (
        This : in Player)
        return Positive;
private
    type Player (Num_Bases : Positive; Num_Units : Natural) is record
        Bases : Coordinate_List (1 .. Num_Bases);
        Unit_Coordinates : Coordinate_List (1 .. Num_Units);
        Unit_Types : Type_List (1 .. Num_Units);
        Team : Positive;
        Productions : Natural;
    end record;
end Players;
