with Actions;
with Boards;
with Ada.Unchecked_Deallocation;

package body Servers is
    type Action_Array_Ptr is access Actions.Action_Array;

    type Player_Data_State is
        record
            Team : Natural;
            State : Player_Activity;
            Acts : Action_Array_Ptr;
        end record;
    type Player_Data_State_Array is array (Positive range <>) of
        Player_Data_State;
    type Player_Data_State_Array_Ptr is access Player_Data_State_Array;

    procedure Free_Action_Array is new Ada.Unchecked_Deallocation (
        Actions.Action_Array, Action_Array_Ptr);
    procedure Free_Player_Data is new Ada.Unchecked_Deallocation (
        Player_Data_State_Array, Player_Data_State_Array_Ptr);

    task body Server is
        The_Board : Boards.Board_Ptr;
        Players : access Player_Data_State_Array;
        Has_Changed : Boolean;

        procedure Was_Changed is
            --  Accept any and all waiters
            loop
                select
                    accept Wait_For_Change;
                else
                    exit;
                end select;
            end loop;
            Has_Changed := True;
        end Was_Changed;

        procedure Apply_All_Actions is
            Max_Actions : Natural;
        begin
            Max_Actions := 0;
            for Index in Players'Range loop
                Max_Actions := Max_Actions + Players (Index).Acts'Length;
            end loop;

            declare
                All_Actions : Actions.Action_Array (1 .. Max_Actions);
                Last_Index : Natural;
            begin
                Last_Index := 0;
                for Index in Players'Range loop
                    for I in Players (Index).Acts'Range loop
                        if Boards.Is_Valid (
                            The_Board.all,
                            Players (Index).Acts (I))
                        then
                            Last_Index := Last_Index + 1;
                            All_Actions (Last_Index) :=
                                Players (Index).Acts (I);
                        end if;
                    end loop;
                end loop;

                Boards.Apply_Actions (The_Board.all, All_Actions);
            end;
        end Apply_All_Actions;
    begin
        accept Create (Board : in Boards.Board) do
            The_Board := new Boards.Board (Board.Width, Board.Height);
            The_Board.all := Board;
            declare
                Players : Boards.Player_List := Boards.Get_Players (Board);
            begin
                The_Players := new Player_Data_State_Array
                    (1 ..  Players'Length);
                for Index in Players'Range loop
                    The_Players (Index) := (Players (Index), EMPTY, null);
                end loop;
                Was_Changed;
            end;
        end Create;

        loop
            select
                accept Connect (Who : out Natural) do
                    for Index in Players'Range loop
                        if Players (Index).State := EMPTY then
                            Players (Index).State := ACTIVE;
                            Who := Players (Index).Team;
                            Was_Changed;
                            exit;
                        end if;
                    end loop;
                end Connect;
            or
                accept Disconnect (Who : in Natural) do
                    for Index in Players'Range loop
                        if Players (Index).Team = Who then
                            Players (Index).State := DISCONNECTED;
                            Was_Changed;
                            exit;
                        end if;
                    end loop;
                end Disconnect;
            or
                accept Get_Dimensions (
                    Width : out Positive;
                    Height : out Positive;
                    Player_Count : out Positive) do
                    Width := The_Board.Width;
                    Height := The_Board.Height;
                    Player_Count := Players'Length;
                end Get_Dimensions;
            or
                accept Commit (
                    Which : in Actions.Action_Array;
                    Who : in Natural) do
                    declare
                        Any_Holdouts : Boolean;
                    begin
                        Any_Holdouts := False;
                        for Index in Players'Range loop
                            if Players (Index).Team = Who then
                                Players (Index).Acts := new Actions.Action_Array
                                    (1 .. Which'Length);
                                Players (Index).State := COMMITTED;
                                Was_Changed;
                            end if;
                            if Players (Index).State /= COMMITTED then
                                Any_Holdouts := True;
                            end if;
                        end loop;

                        if Any_Holdouts = False then
                            Was_Changed;
                            Apply_All_Actions;
                        end if;
                    end;
                end Commit;
            or
                accept Uncommit (Who : in Natural) do
                    for Index in Players'Range loop
                        if Players (Index).Team = Who then
                            if Players (Index).Acts /= null then
                                Free_Action_Array (Players (Index).Acts);
                            end if;
                            Players (Index).State := ACTIVE;
                            Was_Changed;
                        end if;
                    end loop;
                end Uncommit;
            or
                accept Quit (Who : in Natural) do
                    for Index in Players'Range loop
                        if Players (Index).Team = Who then
                            Players (Index).State := QUIT;
                            Was_Changed;
                            exit;
                        end if;
                    end loop;
                end Quit;
            or
                accept Kill do
                    for Index in Players'Range loop
                        Players (Index).State := QUIT;
                    end loop;
                    Was_Changed;

                    for Index in Players'Range loop
                        if Players (Index).Acts /= null then
                            Free_Action_Array (Players (Index).Acts);
                        end if;
                    end loop;
                    Free_Player_Data (Players);
                    Boards.Free_Board (The_Board);
                    Was_Changed;
                end Kill;
            or
                accept Check (Changed : out Boolean) do
                    Changed := Has_Changed;
                    Has_Changed := False;
                end Check;
            or
                accept Query (Who : in Natural;
                    Board : out Boards.Board;
                    Statuses : out Player_Status_Array) do
                    Board := Boards.Localize (The_Board.all, Who);
                    for Index in Players'Range loop
                        Statuses (I) := (Players (I).Team, Players (I).State);
                    end loop;
                end Query;
            end select;
        end loop;
    end Server;

end Servers;
