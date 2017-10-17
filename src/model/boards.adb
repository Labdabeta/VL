with Units;
with Tiles;

package body Boards is

    procedure Apply_Actions (
        This : in out Board;
        Action_List : in Actions.Action_Array) is

    begin

        --  REMEMBER: Spawn/Infect take place in a 'generation' phase AFTER
        --  movement resolution. Also you can't generate on an occupied square.
        null;
        -- TODO

    end Apply_Actions;

    function Create_Board (
        Width, Height : in Positive)
        return Board is
        New_Board : Board := (
            Width => Width,
            Height => Height,
            Contents => (others => (others => (
                (Units.UNKNOWN, 1), Tiles.UNKNOWN))));
    begin
        return New_Board;
    end Create_Board;

    function Get_Actions_From (
        This : in Board;
        From : in Coordinates.Coordinate)
        return Actions.Action_Array is
        Empty_Array : Actions.Action_Array (1 .. 0) := (
            others => ((1, 1), (1, 1), Actions.MOVE, Units.VAMPIRE, 1));
    begin
        return Empty_Array;
    end Get_Actions_From;

    function Get_Tile (
        This : in Board;
        From : in Coordinates.Coordinate) return Tiles.Tile is

    begin

        return ((Units.UNKNOWN, 1), Tiles.UNKNOWN);
        -- TODO

    end Get_Tile;

    function Get_Winner (
        This : in Board)
        return Natural is

    begin

        return 0;
        -- TODO

    end Get_Winner;

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Board is

    begin

        return Create_Board (1, 1);
        -- TODO

    end Input;

    function Is_Valid (
        This : in Board;
        What : in Actions.Action)
        return Boolean is
    begin
        return False;
    end Is_Valid;

    function Localize (
        This : in Board;
        Team : in Positive)
        return Board is

    begin

        return Create_Board (1, 1);
        -- TODO

    end Localize;

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Board) is

    begin

        null;
        -- TODO

    end Output;

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Board) is

    begin

        null;
        -- TODO

    end Read;

    procedure Set_Tile (
        This : in out Board;
        From : in Coordinates.Coordinate;
        To : in Tiles.Tile) is

    begin

        null;
        -- TODO

    end Set_Tile;

    function To_String (
        This : in Board)
        return String is

    begin

        return "";
        -- TODO

    end To_String;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Board) is

    begin

        null;
        -- TODO

    end Write;

end Boards;
