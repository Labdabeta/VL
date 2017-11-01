with Actions;
with Boards;
with Constants;
with Lobby;
with VL;
with GNAT.Sockets; use GNAT.Sockets;

package body Clients is
    --  Poll at 10hz
    Poll_Delay : constant Duration := 0.1;

    task body Client is
        Connection : Stream_Access;
        Server_Socket : Socket_Type;
        Server_Address : Sock_Addr_Type;
        Read_Selector : Selector_Type;
        Server_Set : Socket_Set_Type;
        Empty_Set : Socket_Set_Type;
        Read_Status : Selector_Status;
        Team : Natural;
        Created : Boolean := False;
        Temp_Lobby : Lobby.Lobby_Element;
        Is_Killed : Boolean := False;
        Non_Blocking_Request : Request_Type (Non_Blocking_IO) := (
                Name => Non_Blocking_IO,
                Enabled => True);
        Game_Over : Boolean;
    begin
        select
            accept Initialize (The_Lobby : in Lobby.Lobby_Element) do
                Temp_Lobby := The_Lobby;
            end Initialize;
        or
            accept Kill do
                Is_Killed := True;
            end Kill;
        end select;

        if not Is_Killed then
            Server_Address.Addr := Addresses (
                Get_Host_By_Name (Temp_Lobby.Address), 1);
            Server_Address.Port := Constants.VL_Port;

            Create_Socket (Server_Socket);
            Set_Socket_Option (
                Server_Socket, Socket_Level, (Reuse_Address, True));

            Connect_Socket (Server_Socket, Server_Address);

            Connection := Stream (Server_Socket);
            Control_Socket (Server_Socket, Non_Blocking_Request);

            Empty (Empty_Set);
            Empty (Server_Set);
            Set (Server_Set, Server_Socket);
            Create_Selector (Read_Selector);

            Natural'Read (Connection, Team);
            Game_Over := False;
            while not Game_Over loop
                select
                    accept Commit (Which : in Actions.Action_Array) do
                        Character'Write (Connection, 'C');
                        Actions.Action_Array'Output (Connection, Which);
                    end Commit;
                or
                    accept Uncommit do
                        Character'Write (Connection, 'U');
                    end Uncommit;
                or
                    accept Kill do
                        Shutdown_Socket (Server_Socket);
                        Game_Over := True;
                    end Kill;
                else
                    Check_Selector (Read_Selector,
                        Server_Set, Empty_Set,
                        Read_Status, Poll_Delay);
                    if Read_Status = Completed then
                        declare
                            New_Board : Boards.Board :=
                                Boards.Board'Input (Connection);
                            New_Actions : Actions.Action_Array :=
                                Actions.Action_Array'Input (Connection);
                            Num_Players : Natural := Natural'Input (Connection);
                            New_State : VL.VL_State (
                                Width => New_Board.Width,
                                Height => New_Board.Height,
                                Num_Players => Num_Players,
                                Num_Actions => New_Actions'Length);
                            Query : Character;
                        begin
                            New_State.Board := New_Board;
                            New_State.Last_Actions := New_Actions;
                            for I in New_State.Players'Range loop
                                New_State.Players (I).Team :=
                                    Natural'Input (Connection);
                                Character'Read (Connection, Query);
                                case Query is
                                    when 'E' => New_State.Players (I).Status :=
                                        VL.EMPTY;
                                    when 'A' => New_State.Players (I).Status :=
                                        VL.ACTIVE;
                                    when 'C' => New_State.Players (I).Status :=
                                        VL.COMMITTED;
                                    when 'Q' => New_State.Players (I).Status :=
                                        VL.QUIT;
                                    when others =>
                                        New_State.Players (I).Status := VL.QUIT;
                                end case;
                            end loop;

                            if Boards.Get_Winner (New_State.Board) /= 0 then
                                Game_Over := True;
                            end if;
                            Game.Set (New_State);
                        end;
                    end if;
                end select;
            end loop;
        end if;
    end Client;
end Clients;
