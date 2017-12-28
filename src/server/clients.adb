with Actions;
with Boards;
with Constants;
with Lobby;
with VL;
with GNAT.Sockets; use GNAT.Sockets;

with Ada.Text_IO;

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
        Address_Length : Natural;
    begin
        -- No need to create the game, as it will be loaded by a set
        select
            accept Initialize (The_Lobby : in Lobby.Lobby_Element) do
                Temp_Lobby := The_Lobby;
                -- EXECUTED!
            end Initialize;
        or
            accept Kill do
                Is_Killed := True;
            end Kill;
        end select;

        if not Is_Killed then
            Address_Length := Temp_Lobby.Address'First;
            while Address_Length < Temp_Lobby.Address'Last and
                Temp_Lobby.Address (Address_Length) /= ' '
            loop
                Address_Length := Address_Length + 1;
            end loop;
            Address_Length := Address_Length - 1;

            Ada.Text_IO.Put_Line ("Host is '" &
                Temp_Lobby.Address (Temp_Lobby.Address'First .. Address_Length)
                & "'");
            Server_Address.Addr := Addresses (
                Get_Host_By_Name (Temp_Lobby.Address (
                    Temp_Lobby.Address'First .. Address_Length)), 1);
            Server_Address.Port := Constants.VL_Client_Port;

            Create_Socket (Server_Socket);
            Set_Socket_Option (
                Server_Socket, Socket_Level, (Reuse_Address, True));

            -- Never executed
            Ada.Text_IO.Put_Line ("Connecting...");
            Connect_Socket (Server_Socket, Server_Address);
            Ada.Text_IO.Put_Line ("Connected!");

            Connection := Stream (Server_Socket);
            Control_Socket (Server_Socket, Non_Blocking_Request);

            Empty (Empty_Set);
            Empty (Server_Set);
            Set (Server_Set, Server_Socket);
            Create_Selector (Read_Selector);

            -- Never executed
            Ada.Text_IO.Put_Line ("Reading team...");
            Natural'Read (Connection, Team);
            Ada.Text_IO.Put_Line ("Read team as " & Natural'Image (Team));
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
                    -- This is never executed
                    Ada.Text_IO.Put_Line ("Checking...");
                    Check_Selector (Read_Selector,
                        Server_Set, Empty_Set,
                        Read_Status, Poll_Delay);
                    if Read_Status = Completed then
                        Ada.Text_IO.Put_Line ("Read!");
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
