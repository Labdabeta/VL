with Units;

package Tiles is
    type Tile_Kind is (BASE, PIT, FLOOR, PRODUCER, IMPASSABLE, UNKNOWN);
    for Tile_Kind use (
        BASE        => 0,
        PIT         => 1,
        FLOOR       => 2,
        PRODUCER    => 3,
        IMPASSABLE  => 4,
        UNKNOWN     => 5);
    subtype Real_Tile_Kind is Tile_Kind range BASE .. IMPASSABLE;

    type Tile_Occupant is record
        Unit : Units.Occupant;
        Team : Natural;
    end record;

    type Presence_Array is array (Positive range <>) of Tile_Occupant;

    type Tile is record
        Occupant : Tile_Occupant;
        Kind : Tile_Kind;
    end record;

    procedure Resolve (
        This : in out Tile;
        Presence : in Presence_Array;
        Create : in Boolean)
        with Post => (if Create then This.Kind /= PIT);

    function To_String (
        This : in Tile)
        return String;

end Tiles;
