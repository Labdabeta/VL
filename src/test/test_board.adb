with Test_Suites; use Test_Suites;
with Boards; use Boards;
with Tiles; use Tiles;
with Units; use Units;
with Actions; use Actions;

package body Test_Board is
    function Small_Board return Board;
    function Small_Board_Localized return Board;
    function Small_Board_Post return Board;
    function Small_Board_Post_Post return Board;
    function Small_Board_Win return Board;
    function Small_Board_Actions return Action_Array;
    function Small_Board_Post_Actions return Action_Array;
    function Small_Board_Post_Bad_Actions return Action_Array;
    function Board_Equality (A, B : in Board) return Boolean;

    procedure Action_Test is
        Initial : Board := Small_Board;
        Next : Board := Small_Board_Post;
        Final : Board := Small_Board_Post_Post;
        Events : Action_Array := Small_Board_Actions;
        Post_Events : Action_Array := Small_Board_Post_Actions;
        Bad_Events : Action_Array := Small_Board_Post_Bad_Actions;
    begin
        for Index in Events'Range loop
            Assert (
                Is_Valid (Initial, Events (Index)),
                "Action: " & To_String (Events (Index)) &
                " should have been valid on:" & ASCII.LF &
                To_String (Initial));
        end loop;

        Apply_Actions (Initial, Events);
        Assert (
            Board_Equality (Initial, Next),
            "Expected: " & ASCII.LF &
            To_String (Next) & ASCII.LF & "Got: " & ASCII.LF &
            To_String (Initial));

        for Index in Post_Events'Range loop
            Assert (
                Is_Valid (Next, Post_Events (Index)),
                "Action: " & To_String (Post_Events (Index)) &
                " should have been valid on:" & ASCII.LF &
                To_String (Next));
        end loop;

        Apply_Actions (Next, Post_Events);
        Assert (
            Board_Equality (Next, Final),
            "Expected: " & ASCII.LF &
            To_String (Final) & ASCII.LF & "Got: " & ASCII.LF &
            To_String (Next));

        for Index in Bad_Events'Range loop
            Assert (
                not Is_Valid (Final, Bad_Events (Index)),
                "Action: " & To_String (Bad_Events (Index)) &
                " should have been invalid on:" & ASCII.LF &
                To_String (Final));
        end loop;

    end Action_Test;

    procedure Assert_Equal_Action_Set (
        First : in Action_Array;
        Second : in Action_Array;
        Message : in String) is
        Found : Boolean;
    begin
        if First'Length /= Second'Length then
            Assert (False, Message);
        end if;

        for First_Index in First'Range loop
            Found := False;
            for Second_Index in Second'Range loop
                if First (First_Index) = Second (Second_Index) then
                    Found := True;
                end if;
            end loop;
            if not Found then
                Assert (False, Message);
            end if;
        end loop;
        Assert (True, Message);
    end Assert_Equal_Action_Set;

    procedure Assert_Equal_Tiles is new Assert_Same (
        Value_Type => Tile,
        To_String => To_String);

    procedure Assert_Board_IO is new Assert_IO (
        Any_Type => Board,
        To_String => To_String);

    procedure Basic_Test is
        Test_Board : Board := Create_Board (1, 1);
    begin
        Set_Tile (Test_Board, (1, 1), ((UNKNOWN, 0), UNKNOWN));
        Assert_Equal_Tiles (
            Get_Tile (Test_Board, (1, 1)),
            ((UNKNOWN, 0), UNKNOWN));

        Assert (To_String (Test_Board) = ASCII.ESC & "[44m" & ASCII.ESC & "[30m"
            & "0" & ASCII.ESC & "[39m" & ASCII.ESC & "49m" & ASCII.LF,
            "Wrong 1x1 board string.");

        Assert (
            Board_Equality (Localize (Small_Board, 1), Small_Board_Localized),
            "Localization failed. Expected: " & ASCII.LF &
            To_String (Small_Board_Localized) & ASCII.LF & "Got: " & ASCII.LF &
            To_String (Localize (Small_Board, 1)));
    end Basic_Test;

    function Board_Equality (A, B : in Board) return Boolean is
    begin
        if A.Width /= B.Width or A.Height /= B.Height then
            return False;
        end if;

        for Y in Positive range 1 .. A.Height loop
            for X in Positive range 1 .. A.Width loop
                if Get_Tile (A, (X, Y)) /= Get_Tile (B, (X, Y)) then
                    return False;
                end if;
            end loop;
        end loop;

        return True;
    end Board_Equality;

    procedure IO_Test is
        Test_Board : Board := Small_Board;
    begin
        Assert_Board_IO (Test_Board);
    end IO_Test;

    function Small_Board return Board is
        Result : Board := Create_Board (5, 5);
    begin
        Set_Tile (Result, (1, 1), ((VAMPIRE, 1), FLOOR));
        Set_Tile (Result, (1, 2), ((LEPRECHAUN, 1), FLOOR));
        Set_Tile (Result, (1, 3), ((HUMAN, 1), FLOOR));
        Set_Tile (Result, (1, 4), ((ZOMBIE, 1), FLOOR));
        Set_Tile (Result, (1, 5), ((FAIRY, 1), FLOOR));
        Set_Tile (Result, (2, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (2, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 3), ((NONE, 1), BASE));
        Set_Tile (Result, (2, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 5), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 3), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (4, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 3), ((NONE, 2), BASE));
        Set_Tile (Result, (4, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 5), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (5, 1), ((VAMPIRE, 2), FLOOR));
        Set_Tile (Result, (5, 2), ((LEPRECHAUN, 2), FLOOR));
        Set_Tile (Result, (5, 3), ((HUMAN, 2), FLOOR));
        Set_Tile (Result, (5, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (5, 5), ((FAIRY, 2), FLOOR));
        return Result;
    end Small_Board;

    function Small_Board_Actions return Action_Array is
        Results : Action_Array := (
            1 => ((1, 1), (2, 1), MOVE, VAMPIRE, 1),
            2 => ((1, 2), (2, 2), MOVE, LEPRECHAUN, 1),
            3 => ((1, 3), (2, 3), MOVE, HUMAN, 1),
            4 => ((1, 4), (2, 4), MOVE, ZOMBIE, 1),
            5 => ((1, 5), (2, 5), MOVE, FAIRY, 1),
            6 => ((5, 1), (3, 1), MOVE, VAMPIRE, 2),
            7 => ((5, 2), (4, 3), CREATE, LEPRECHAUN, 2),
            8 => ((5, 3), (4, 3), DESTROY, HUMAN, 2),
            9 => ((5, 4), (4, 4), INFECT, ZOMBIE, 2),
            10 => ((5, 5), (4, 5), MOVE, FAIRY, 2));
    begin
        return Results;
    end Small_Board_Actions;

    function Small_Board_Localized return Board is
        Result : Board := Create_Board (5, 5);
    begin
        Set_Tile (Result, (1, 1), ((VAMPIRE, 1), FLOOR));
        Set_Tile (Result, (1, 2), ((LEPRECHAUN, 1), FLOOR));
        Set_Tile (Result, (1, 3), ((HUMAN, 1), FLOOR));
        Set_Tile (Result, (1, 4), ((ZOMBIE, 1), FLOOR));
        Set_Tile (Result, (1, 5), ((FAIRY, 1), FLOOR));
        Set_Tile (Result, (2, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (2, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 3), ((NONE, 1), BASE));
        Set_Tile (Result, (2, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 5), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 3), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (4, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 3), ((NONE, 2), BASE));
        Set_Tile (Result, (4, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 5), ((UNKNOWN, 0), UNKNOWN));
        Set_Tile (Result, (5, 1), ((UNKNOWN, 0), UNKNOWN));
        Set_Tile (Result, (5, 2), ((UNKNOWN, 0), UNKNOWN));
        Set_Tile (Result, (5, 3), ((UNKNOWN, 0), UNKNOWN));
        Set_Tile (Result, (5, 4), ((UNKNOWN, 0), UNKNOWN));
        Set_Tile (Result, (5, 5), ((UNKNOWN, 0), UNKNOWN));
        return Result;
    end Small_Board_Localized;

    function Small_Board_Post return Board is
        Result : Board := Create_Board (5, 5);
    begin
        Set_Tile (Result, (1, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 3), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 1), ((VAMPIRE, 1), PRODUCER));
        Set_Tile (Result, (2, 2), ((LEPRECHAUN, 1), FLOOR));
        Set_Tile (Result, (2, 3), ((HUMAN, 1), BASE));
        Set_Tile (Result, (2, 4), ((ZOMBIE, 1), FLOOR));
        Set_Tile (Result, (2, 5), ((FAIRY, 1), PRODUCER));
        Set_Tile (Result, (3, 1), ((VAMPIRE, 2), FLOOR));
        Set_Tile (Result, (3, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 3), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (4, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 3), ((NONE, 2), BASE));
        Set_Tile (Result, (4, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (4, 5), ((FAIRY, 2), PRODUCER));
        Set_Tile (Result, (5, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (5, 2), ((LEPRECHAUN, 2), FLOOR));
        Set_Tile (Result, (5, 3), ((HUMAN, 2), FLOOR));
        Set_Tile (Result, (5, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (5, 5), ((NONE, 0), FLOOR));
        return Result;
    end Small_Board_Post;

    function Small_Board_Post_Actions return Action_Array is
        Results : Action_Array := (
            1 => ((3, 4), (3, 4), SPAWN, VAMPIRE, 1),
            2 => ((2, 2), (3, 2), MOVE, LEPRECHAUN, 1),
            3 => ((2, 3), (1, 3), DESTROY, HUMAN, 1),
            4 => ((3, 1), (3, 2), MOVE, VAMPIRE, 2),
            5 => ((4, 4), (3, 4), INFECT, ZOMBIE, 2));
    begin
        return Results;
    end Small_Board_Post_Actions;

    function Small_Board_Post_Bad_Actions return Action_Array is
        Results : Action_Array := (
            1 => ((2, 5), (2, 6), MOVE, FAIRY, 1),
            2 => ((1, 1), (2, 1), MOVE, VAMPIRE, 1),
            3 => ((2, 1), (2, 3), MOVE, VAMPIRE, 1),
            4 => ((3, 2), (4, 2), MOVE, LEPRECHAUN, 2),
            5 => ((10, 10), (10, 11), MOVE, VAMPIRE, 3),
            6 => ((1, 1), (1, 1), SPAWN, VAMPIRE, 1),
            7 => ((2, 2), (2, 2), SPAWN, VAMPIRE, 2));
    begin
        return Results;
    end Small_Board_Post_Bad_Actions;

    function Small_Board_Post_Post return Board is
        Result : Board := Create_Board (5, 5);
    begin
        Set_Tile (Result, (1, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 3), ((NONE, 0), PIT));
        Set_Tile (Result, (1, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 1), ((VAMPIRE, 1), PRODUCER));
        Set_Tile (Result, (2, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 3), ((HUMAN, 1), BASE));
        Set_Tile (Result, (2, 4), ((ZOMBIE, 1), FLOOR));
        Set_Tile (Result, (2, 5), ((FAIRY, 1), PRODUCER));
        Set_Tile (Result, (3, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 2), ((LEPRECHAUN, 1), FLOOR));
        Set_Tile (Result, (3, 3), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (4, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 3), ((NONE, 2), BASE));
        Set_Tile (Result, (4, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (4, 5), ((FAIRY, 2), PRODUCER));
        Set_Tile (Result, (5, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (5, 2), ((LEPRECHAUN, 2), FLOOR));
        Set_Tile (Result, (5, 3), ((HUMAN, 2), FLOOR));
        Set_Tile (Result, (5, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (5, 5), ((NONE, 0), FLOOR));
        return Result;
    end Small_Board_Post_Post;

    function Small_Board_Win return Board is
        Result : Board := Create_Board (5, 5);
    begin
        Set_Tile (Result, (1, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 3), ((NONE, 0), PIT));
        Set_Tile (Result, (1, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (1, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 1), ((VAMPIRE, 1), PRODUCER));
        Set_Tile (Result, (2, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (2, 3), ((HUMAN, 1), BASE));
        Set_Tile (Result, (2, 4), ((ZOMBIE, 1), FLOOR));
        Set_Tile (Result, (2, 5), ((FAIRY, 1), PRODUCER));
        Set_Tile (Result, (3, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 2), ((LEPRECHAUN, 1), FLOOR));
        Set_Tile (Result, (3, 3), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (3, 4), ((NONE, 0), FLOOR));
        Set_Tile (Result, (3, 5), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 1), ((NONE, 0), PRODUCER));
        Set_Tile (Result, (4, 2), ((NONE, 0), FLOOR));
        Set_Tile (Result, (4, 3), ((NONE, 0), BASE));
        Set_Tile (Result, (4, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (4, 5), ((FAIRY, 2), PRODUCER));
        Set_Tile (Result, (5, 1), ((NONE, 0), FLOOR));
        Set_Tile (Result, (5, 2), ((LEPRECHAUN, 2), FLOOR));
        Set_Tile (Result, (5, 3), ((HUMAN, 2), FLOOR));
        Set_Tile (Result, (5, 4), ((ZOMBIE, 2), FLOOR));
        Set_Tile (Result, (5, 5), ((NONE, 0), FLOOR));
        return Result;
    end Small_Board_Win;

    procedure Utility_Test is
        Test_Board : Board := Small_Board;
        Players : Player_List := Get_Players (Test_Board);
        Won_Board : Board := Small_Board_Win;
    begin
        Assert (Get_Winner (Test_Board) = 0,
            "No winner should exist on " & ASCII.LF &
            To_String (Test_Board));
        Assert (Players'Length = 2,
            "There should be two players on " & ASCII.LF &
            To_String (Test_Board));
        Assert (Players (1) = 1 or Players (2) = 1,
            "There should be a player 1 on " & ASCII.LF &
            To_String (Test_Board));
        Assert (Players (1) = 2 or Players (2) = 2,
            "There should be a player 2 on " & ASCII.LF &
            To_String (Test_Board));
        Assert (Get_Winner (Won_Board) = 1,
            "Player 1 should have won on " & ASCII.LF &
            To_String (Test_Board));
    end Utility_Test;
end Test_Board;
