with Actions;
with Boards;
with Constants;
with Lobby;
with VL;
with GNAT.Sockets; use GNAT.Sockets;

package body Servers is
    --  Poll at 10hz
    Poll_Delay : constant Duration := 0.1;

    task type Client_Reader (Game : access VL.VL_Game) is
        entry Initialize (
            Sock : in Socket_Type;
            Id : in Natural);
        entry Kill;
    end Client_Reader;

    task body Client_Reader is
        Team : Natural;
        Connection : Stream_Access;
        Killed : Boolean := False;
        Read_Selector : Selector_Type;
        Client_Set : Socket_Set_Type;
        Empty_Set : Socket_Set_Type;
        Read_Status : Selector_Status;
    begin
        select
            accept Initialize (
                Sock : in Socket_Type;
                Id : in Natural) do
                Connection := Stream (Sock);
                Team := Id;

                Empty (Client_Set);
                Empty (Empty_Set);
                Set (Client_Set, Sock);
                Create_Selector (Read_Selector);
            end Initialize;
        or
            accept Kill do
                Killed := True;
            end Kill;
        or
            terminate;
        end select;

        while not Killed loop
            select
                accept Kill do
                    Close_Selector (Read_Selector);
                    Killed := True;
                end Kill;
            else
                Check_Selector (Read_Selector,
                    Client_Set, Empty_Set,
                    Read_Status, Poll_Delay);
                if Read_Status = Completed then
                    declare
                        Message : Character;
                    begin
                        Character'Read (Connection, Message);
                        case Message is
                            when 'X' =>
                                Game.Leave (Team);
                                Killed := True;
                            when 'C' =>
                                Game.Commit (
                                    Actions.Action_Array'Input (Connection),
                                    Team);
                            when 'U' =>
                                Game.Uncommit (Team);
                            when others => null;
                        end case;
                    end;
                end if;
            end select;
        end loop;
    end Client_Reader;

    task type Client_Writer (Game : access VL.VL_Game) is
        entry Initialize (
            Sock : in Socket_Type;
            Id : out Natural);
        entry Kill;
    end Client_Writer;

    task body Client_Writer is
        Team : Natural;
        Socket : Socket_Type;
        Connection : Stream_Access;
        Killed : Boolean := False;
        Notifier : aliased VL.VL_Notifier;
        Width, Height, Num_Players : Positive;
        Num_Actions : Natural;
        Game_Over : Boolean := False;

        procedure Update (State : in VL.VL_State) is begin
            Boards.Board'Output (Connection,
                Boards.Localize (State.Board, Team));
            Actions.Action_Array'Output (Connection,
                Boards.Localize_Actions (
                    State.Board, Team, State.Last_Actions));
            Natural'Output (Connection, State.Num_Players);
            for Index in State.Players'Range loop
                Natural'Output (Connection, State.Players (Index).Team);
                case State.Players (Index).Status is
                    when VL.EMPTY => Character'Output (Connection, 'E');
                    when VL.ACTIVE => Character'Output (Connection, 'A');
                    when VL.COMMITTED => Character'Output (Connection, 'C');
                    when VL.QUIT => Character'Output (Connection, 'Q');
                end case;
            end loop;
        end Update;
    begin
        select
            accept Initialize (
                Sock : in Socket_Type;
                Id : out Natural) do
                Socket := Sock;
                Game.Join (Team);
                Id := Team;
            end Initialize;
        or
            accept Kill do
                Killed := True;
            end Kill;
        or
            terminate;
        end select;

        if not Killed then
            Connection := Stream (Socket);
            Natural'Write (Connection, Team);
            Game.Attach_Notifier (Notifier'Unchecked_Access);
            while not Game_Over loop
                --  Will wait until change
                Notifier.Get_Dimensions (
                    Width, Height, Num_Players, Num_Actions);
                declare
                    New_State : VL.VL_State (
                        Width, Height, Num_Players, Num_Actions);
                begin
                    Notifier.Get_State (New_State);
                    if Boards.Get_Winner (New_State.Board) /= 0 then
                        Game_Over := True;
                    end if;
                end;
            end loop;
            accept Kill do
                Shutdown_Socket (Socket);
            end Kill;
        end if;
    end Client_Writer;

    task body Server is
        Readers : array (1 .. Player_Count - 1) of Client_Reader (Game);
        Writers : array (1 .. Player_Count - 1) of Client_Writer (Game);
        Server_Socket : Socket_Type;
        Local_Address : Sock_Addr_Type;
        Accept_Selector : Selector_Type;
        Empty_Set : Socket_Set_Type;
        Server_Set : Socket_Set_Type;
        Accept_Status : Selector_Status;
        Team : Natural;
        Is_Killed : Boolean := False;
        Non_Blocking_Request : Request_Type (Non_Blocking_IO) := (
                Name => Non_Blocking_IO,
                Enabled => True);
        Lobby_Id : Positive;
        Temp_Lobby : Lobby.Lobby_Element;
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
            Game.Join (Team);
            Lobby_Id := Lobby.Post_Lobby (Temp_Lobby);
            Local_Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
            Local_Address.Port := Constants.VL_Port;

            Create_Socket (Server_Socket);
            Set_Socket_Option (Server_Socket, Socket_Level,
                (Reuse_Address, True));
            Control_Socket (Server_Socket, Non_Blocking_Request);

            Bind_Socket (Server_Socket, Local_Address);
            Listen_Socket (Server_Socket);

            Empty (Empty_Set);
            Empty (Server_Set);
            Set (Server_Set, Server_Socket);
            Create_Selector (Accept_Selector);
            Accept_Loop : for Index in Readers'Range loop
                loop
                    select
                        accept Kill do
                            Is_Killed := True;
                        end Kill;
                    else
                        Check_Selector (Accept_Selector,
                            Server_Set, Empty_Set, Accept_Status, Poll_Delay);
                        if Accept_Status = Completed then
                            declare
                                Client_Socket : Socket_Type;
                                Client_Address : Sock_Addr_Type;
                                Id : Natural;
                            begin
                                Accept_Socket (Server_Socket, Client_Socket,
                                    Client_Address);
                                Control_Socket (
                                    Client_Socket, Non_Blocking_Request);
                                Writers (Index).Initialize (Client_Socket, Id);
                                Readers (Index).Initialize (Client_Socket, Id);
                            end;
                            exit;
                        end if;
                    end select;
                    if Is_Killed then
                        exit Accept_Loop;
                    end if;
                end loop;
            end loop Accept_Loop;
            Close_Selector (Accept_Selector);

            Lobby.Remove_Post (Lobby_Id);

            while not Is_Killed loop
                select
                    accept Commit (Which : in Actions.Action_Array) do
                        Game.Commit (Which, Team);
                    end Commit;
                or
                    accept Uncommit do
                        Game.Uncommit (Team);
                    end Uncommit;
                or
                    accept Kill do
                        Is_Killed := True;
                    end Kill;
                end select;
            end loop;
        end if;

        --  Kill the tasks even if we never started them!
        for Index in Readers'Range loop
            Readers (Index).Kill;
        end loop;

        for Index in Writers'Range loop
            Writers (Index).Kill;
        end loop;
    end Server;
end Servers;
