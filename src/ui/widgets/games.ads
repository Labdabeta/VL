with Actions;
with Boards;
with Tiles;
with Coordinates;
with SDL;

package Games is
    Max_Players : constant := 16;
    procedure Draw_Board (
        Which : in Boards.Board;
        Where : in SDL.Rectangle);

    procedure Draw_Tile (
        Which : in Tiles.Tile;
        Where : in SDL.Rectangle);

    procedure Draw_Actions (
        Reference : in Boards.Board;
        Which : in Actions.Action_Array;
        Where : in SDL.Rectangle);

    function Get_Mouse_Coord (
        Which : in Boards.Board;
        Where : in SDL.Rectangle) return Coordinates.Coordinate;

    function Get_Rectangle (
        Which : in Boards.Board;
        Where : in SDL.Rectangle;
        Coord : in Coordinates.Coordinate) return SDL.Rectangle;
end Games;
