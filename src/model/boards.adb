with Units;
with Tiles;

package body Boards is

    procedure Apply_Actions (
        This : in out Board;
        Action_List : in Actions.Action_Array) is

    begin

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

    function Get_Player (
        This : in Board;
        Team : in Positive)
        return Players.Player is

        Temp_Bases : Players.Coordinate_List (1 .. 1) := (others => (1, 1));
        Temp_Units : Players.Coordinate_List (1 .. 0) := (others => (1, 1));
        Temp_Types : Players.Type_List (1 .. 0) := (others => Units.VAMPIRE);
    begin

        return Players.New_Player (1, Temp_Bases, Temp_Units, Temp_Types, 0);
        -- TODO

    end Get_Player;

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
