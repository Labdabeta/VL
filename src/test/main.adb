with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Suites; use Test_Suites;
with Test_Board;
with Test_Coordinate;
with Test_Tile;
with Test_Action;

procedure Main is
    Separator : String (1 .. 80) := (others => '~');

    Suites : Test_Suites.Test_Suites := (
        1 => Test_Board.Suite'Access,
        2 => Test_Coordinate.Suite'Access,
        3 => Test_Tile.Suite'Access,
        4 => Test_Action.Suite'Access);

    Success : Boolean := True;

    function Suite_Names (Which : in Integer) return String is begin
        case Which is
            when 1 => return "Board";
            when 2 => return "Coordinate";
            when 3 => return "Tile";
            when 4 => return "Action";
            when others => return "ERROR!!!";
        end case;
    end Suite_Names;
begin
    for I in Suites'Range loop
        Put_Line (Separator);
        Put_Line ("Testing " & Suite_Names (I) & "...");
        if Test_Suites.Run_Suite (Suites (I).all) then
            Put_Line ("PASS");
        else
            Put_Line ("FAIL");
            Set_Exit_Status (Failure);
            Success := False;
        end if;
    end loop;

    Put_Line (Separator);

    if Success then
        Put_Line ("All tests passed.");
    else
        Put_Line ("Failures occurred.");
    end if;
end Main;
