with Coordinates;
with Units;
with Tiles;
with Ada.Streams;

package Actions is
    type Action_Kind is (MOVE, CREATE, DESTROY, INFECT, SPAWN);

    type Action is record
        Source, Target : Coordinates.Coordinate;
        Kind : Action_Kind;
        Unit : Units.Unit;
        Team : Positive;
    end record;

    Is_Move_Actor : constant array (Units.Unit) of Boolean := (
        Units.LEPRECHAUN => True,
        Units.HUMAN => True,
        Units.ZOMBIE => True,
        others => False);

    Move_Action_Type : constant array (Units.Unit) of Action_Kind := (
        Units.LEPRECHAUN => CREATE,
        Units.HUMAN => DESTROY,
        Units.ZOMBIE => INFECT,
        others => MOVE);

    function Is_Valid (
        This : in Action)
        return Boolean;

    type Action_Array is array (Positive range <>) of Action;

    function Get_Actions_From (
        What : in Tiles.Tile;
        Where : in Coordinates.Coordinate;
        Board_Size : in Coordinates.Coordinate)
        return Action_Array;

    function To_String (
        This : in Action)
        return String;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Action);

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Action);

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Action);

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Action;

    for Action'Write use Write;
    for Action'Read use Read;
    for Action'Output use Output;
    for Action'Input use Input;
end Actions;

-- Action stream format:
--
-- 4 bytes: Source.X
-- 4 bytes: Source.Y
-- 4 bytes: Target.X
-- 4 bytes: Target.Y
-- 4 bits: Kind
-- 4 bits: Unit
-- 1 byte: Team
