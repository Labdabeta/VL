with Test_Suites; use Test_Suites;
with Boards;
with Tiles;
with Units;

package body Test_Board is
    procedure Basic_Test is
        Test_Board : Boards.Board (2, 2);
    begin
        Boards.Set_Tile (
            This => Test_Board,
            From => (X => 1, Y => 1),
            To => (
                Occupant => (Unit => Units.NONE, Team => 0),
                Kind => Tiles.PIT));
        Assert (True, "TODO");
    end Basic_Test;
end Test_Board;
