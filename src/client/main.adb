with SDL; use SDL;
with Buttons; use Buttons;
with Games;
with Boards;
with Actions;
with Units; use Units;
with Tiles; use Tiles;

procedure Main is
    function Small_Board return Boards.Board is
        Result : Boards.Board := Boards.Create_Board (5, 5);
    begin
        Boards.Set_Tile (Result, (1, 1), ((VAMPIRE, 1), FLOOR));
        Boards.Set_Tile (Result, (1, 2), ((LEPRECHAUN, 1), FLOOR));
        Boards.Set_Tile (Result, (1, 3), ((HUMAN, 1), FLOOR));
        Boards.Set_Tile (Result, (1, 4), ((ZOMBIE, 1), FLOOR));
        Boards.Set_Tile (Result, (1, 5), ((FAIRY, 1), FLOOR));
        Boards.Set_Tile (Result, (2, 1), ((NONE, 0), PRODUCER));
        Boards.Set_Tile (Result, (2, 2), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (2, 3), ((NONE, 1), BASE));
        Boards.Set_Tile (Result, (2, 4), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (2, 5), ((NONE, 0), PRODUCER));
        Boards.Set_Tile (Result, (3, 1), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (3, 2), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (3, 3), ((NONE, 0), PRODUCER));
        Boards.Set_Tile (Result, (3, 4), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (3, 5), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (4, 1), ((NONE, 0), PRODUCER));
        Boards.Set_Tile (Result, (4, 2), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (4, 3), ((NONE, 2), BASE));
        Boards.Set_Tile (Result, (4, 4), ((NONE, 0), FLOOR));
        Boards.Set_Tile (Result, (4, 5), ((NONE, 0), PRODUCER));
        Boards.Set_Tile (Result, (5, 1), ((VAMPIRE, 2), FLOOR));
        Boards.Set_Tile (Result, (5, 2), ((LEPRECHAUN, 2), FLOOR));
        Boards.Set_Tile (Result, (5, 3), ((HUMAN, 2), FLOOR));
        Boards.Set_Tile (Result, (5, 4), ((ZOMBIE, 2), FLOOR));
        Boards.Set_Tile (Result, (5, 5), ((FAIRY, 2), FLOOR));
        return Result;
    end Small_Board;

    function Small_Board_Actions return Actions.Action_Array is
        Results : Actions.Action_Array := (
            1 => ((1, 1), (2, 1), Actions.MOVE, VAMPIRE, 1),
            2 => ((1, 2), (2, 2), Actions.MOVE, LEPRECHAUN, 1),
            3 => ((1, 3), (2, 3), Actions.MOVE, HUMAN, 1),
            4 => ((1, 4), (2, 4), Actions.MOVE, ZOMBIE, 1),
            5 => ((1, 5), (2, 5), Actions.MOVE, FAIRY, 1),
            6 => ((5, 1), (3, 1), Actions.MOVE, VAMPIRE, 2),
            7 => ((5, 2), (4, 3), Actions.CREATE, LEPRECHAUN, 2),
            8 => ((5, 3), (4, 3), Actions.DESTROY, HUMAN, 2),
            9 => ((5, 4), (4, 4), Actions.INFECT, ZOMBIE, 2),
            10 => ((5, 5), (4, 5), Actions.MOVE, FAIRY, 2));
    begin
        return Results;
    end Small_Board_Actions;

    A_Board : Boards.Board := Small_Board;
    Acts : Actions.Action_Array := Small_Board_Actions;
begin
    if not Initialize ("Vampire Leprechauns", 1024, 768) then
        return;
    end if;

    Games.Initialize;

        Begin_Draw;
        Games.Draw_Board (A_Board, (0, 0, 1024, 768));
        Games.Draw_Actions (A_Board, Acts, (0, 0, 1024, 768));
        End_Draw;
    loop
        declare
            E : Event := Step;
        begin
            if E.Kind = QUIT_EVENT then
                exit;
            end if;
        end;
    end loop;

    Games.Finalize;
    Finalize;
end Main;
