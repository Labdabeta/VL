with Actions;
with Ada.Streams;
with Coordinates;
with Players;
with Tiles;

package Boards is
    type Board (Width, Height : Positive) is private;

    function Create_Board (
        Width, Height : in Positive)
        return Board;

    procedure Set_Tile (
        This : in out Board;
        From : in Coordinates.Coordinate;
        To : in Tiles.Tile);

    function Get_Tile (
        This : in Board;
        From : in Coordinates.Coordinate)
        return Tiles.Tile
        with Pre => (From.X <= This.Width and From.Y <= This.Height);

    --  Dynamically builds the appropriate player
    function Get_Player (
        This : in Board;
        Team : in Positive)
        return Players.Player;

    procedure Apply_Actions (
        This : in out Board;
        Action_List : in Actions.Action_Array);

    -- Returns 0 if no winner yet
    function Get_Winner (
        This : in Board)
        return Natural;

    function Localize (
        This : in Board;
        Team : in Positive)
        return Board;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Board);

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Board);

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Board);

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Board;

    function To_String (
        This : in Board)
        return String;

    for Board'Write use Write;
    for Board'Read use Read;
    for Board'Output use Output;
    for Board'Input use Input;
private
    type Tile_Matrix is array (Positive range <>, Positive range <>)
        of Tiles.Tile;

    type Board (Width, Height : Positive) is record
            Contents : Tile_Matrix (1 .. Width, 1 .. Height);
    end record;
end Boards;

-- Board stream format:
--
-- TODO
