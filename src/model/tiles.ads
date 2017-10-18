with Units;
with Ada.Streams;

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

    Max_Collisions : constant := 13;
    type Presence_Array is array (1 .. Max_Collisions) of Tile_Occupant;

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

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Tile);

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Tile);

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Tile);

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Tile;

    for Tile'Write use Write;
    for Tile'Read use Read;
    for Tile'Output use Output;
    for Tile'Input use Input;
end Tiles;

-- Tile stream format:
--
-- 4 bits: Kind
-- 4 bits: Occupant
-- 1 byte: Team
