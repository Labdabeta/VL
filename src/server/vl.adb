with Actions;
with Boards;
with Servers;
with Clients;
with Constants;
with Lobby;

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Paths;

with GNAT.Sockets; use GNAT.Sockets;

package body VL is
    function Load_Map_From_Name (Which : in String)
        return Boards.Board is
        Real_Length : Natural := 1;
        The_File : Ada.Text_IO.File_Type;
        The_Stream : Ada.Text_IO.Text_Streams.Stream_Access;
    begin
        while Which (Real_Length) /= ' ' loop
            Real_Length := Real_Length + 1;
        end loop;

        Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File,
            Paths.Map_Prefix & Which (1 .. Real_Length) & ".vl");
        The_Stream := Ada.Text_IO.Text_Streams.Stream (The_File);
        declare
            Board : Boards.Board := Boards.Board'Input (Stream);
        begin
            Ada.Text_IO.Close (The_File);
            return Board;
        end;
    end Load_Map_From_Name;

    task body VL_Game is
        Is_Host : Boolean;
        Key_String : String (1 .. 120);
        Max_Players : Positive;

        procedure Run_Client (
            Host : String;
            Player_Count : Positive) is
            Server : Socket_Type;
            Address : Sock_Addr_Type;
            Data : Stream_Type;
            Diffed : Boolean;
            Read_Selection : Selector;
            Server_Set : Socket_Set_Type;
            Empty_Set : Socket_Set_Type;
            Read_Status : Selector_Status;
            Read_Delay : constant Duration := 0.1; -- poll at 10Hz

            procedure Load_Statuses (
                Statuses : in out Servers.Player_Data_State_Array) is
                Buffer : Character;
            begin
                for Index in Statuses'Range loop
                    Buffer := Character'Input (Data);
                    case Buffer is
                        when 'E' => Statuses (Index) := Servers.EMPTY;
                        when 'A' => Statuses (Index) := Servers.ACTIVE;
                        when 'C' => Statuses (Index) := Servers.COMMITTED;
                        when 'D' => Statuses (Index) := Servers.DISCONNECTED;
                        when 'Q' => Statuses (Index) := Servers.QUIT;
                        when 'S' => Statuses (Index) := Servers.SELF;
                    end case;
                end Load_Statuses;
        begin
            Address.Addr := Addresses (Get_Host_By_Name (Host), 1);
            Address.Port := Constants.VL_Port;

            Create_Socket (Server);
            Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
            Control_Socket (Server, (Non_Blocking_IO, True));

            Connect_Socket (Server, Address);
            Data := Stream (Server);

            Empty (Client_Set);
            Empty (Empty_Set);
            Set (Client_Set, Sock);
            Create_Selector (Read_Selection);

            declare
                The_Board : Boards.Board := Boards.Board'Input (Data);
                Num_Players : Natural := Natural'Input (Data);
                States : Servers.Player_Data_State_Array (Num_Players);
            begin
                Load_Statuses (States);
                Diffed := True;
                loop
                    select
                        accept Get_Dimensions (
                            Width : out Positive;
                            Height : out Positive;
                            Player_Count : out Positive) do
                            Width := Board.Width;
                            Height := Board.Height;
                            Player_Count := Num_Players;
                        end Get_Dimensions;
                    or
                        accept Commit (Which : in Actions.Action_Array) do
                            Character'Write (Data, 'C');
                            Actions.Action_Array'Output (Data, Which);
                        end Commit;
                    or
                        accept Uncommit do
                            Character'Write (Data, 'U');
                        end Uncommit;
                    or
                        accept Check (Changed : out Boolean) do
                            Changed := Diffed;
                        end Check;
                    or
                        accept Query (
                            Board : out Boards.Board;
                            Statuses : out Servers.Player_Status_Array) do
                            Board := The_Board;
                            Statuses := States;
                        end Query;
                    or
                        accept Quit do
                            Character'Write (Data, 'X');
                            Shutdown_Socket (Server);
                            exit;
                        end Quit;
                    or
                        accept Kill do
                            Shutdown_Socket (Server);
                            exit;
                        end Kill;
                    else
                        Check_Selector (Read_Selection,
                            Client_Set, Empty_Set,
                            Read_Status, Read_Delay);
                        if Read_Status = Completed then
                            The_Board := Boards.Board'Input (Data);
                            Num_Players := Natural'Input (Data);
                            Load_Statuses (States);
                        end if;
                end select;
            end loop;
            Close_Selector (Read_Selection);
        end Run_Client;

        procedure Run_Host (
            Map_Name : String;
            Player_Count : Positive) is
            Server : Socket_Type;
            Address : Sock_Addr_Type;

            Server_Task : Servers.Server;
            Writers : array (1 .. Player_Count - 1) of Clients.Client_Writer;
            Readers : array (1 .. Player_Count - 1) of Clients.Client_Reader;
            Us : Natural;

            Accept_Selection : Selector;
            Server_Set : Socket_Set_Type;
            Accept_Status : Selector_Status;
            Accept_Delay : constant Duration := 0.1; -- poll at 10Hz
        begin
            Server_Task.Create (Load_Map_From_Name (Map_Name));
            Server_Task.Connect (Us);

            Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
            Address.Port := Constants.VL_Port;

            Create_Socket (Server);
            Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
            Control_Socket (Server, (Non_Blocking_IO, True));

            Bind_Socket (Server, Address);
            Listen_Socket (Server);

            Empty (Server_Set);
            Set (Server_Set, Server);
            Create_Selector (Accept_Selection);
            for Index in Writers'Range loop
                loop
                    select
                        accept Kill do
                            Server_Task.Kill;
                            for I in Positive range 1 .. Index loop
                                Readers (I).Kill;
                            end loop;
                            return;
                        end Kill;
                    else
                        Check_Selector (Accept_Selection,
                            Server_Set, Server_Set,
                            Accept_Status, Accept_Delay);
                        if Accept_Status = Completed then
                            declare
                                Client_Sock : Socket_Type;
                                Temp : Sock_Addr_Type;
                                Id : Natural;
                            begin
                                Accept_Socket (Server, Client_Sock, Temp);
                                Control_Socket (
                                    Client_Sock, (Non_Blocking_IO, True));
                                Writers (Index).Create (
                                    Server_Task, Client_Sock, Id);
                                Readers (Index).Create (
                                    Server_Task, Client_Sock, Id);
                            end;
                            exit;
                        end if;
                    end select;
                end loop;
            end loop;
            Close_Selector (Accept_Selection);

            loop
                select
                    accept Get_Dimensions (
                        Width : out Positive;
                        Height : out Positive;
                        Player_Count : out Positive) do
                        Server_Task.Get_Dimensions (
                            Width, Height, Player_Count);
                    end Get_Dimensions;
                or
                    accept Commit (Which : in Actions.Action_Array) do
                        Server_Task.Commit (Which, Us);
                    end Commit;
                or
                    accept Uncommit do
                        Server_Task.Uncommit (Us);
                    end Uncommit;
                or
                    accept Check (Changed : out Boolean) do
                        Server_Task.Check (Changed);
                    end Check;
                or
                    accept Query (
                        Board : out Boards.Board;
                        Statuses : out Servers.Player_Status_Array) do
                        Server_Task.Query (Us, Board, Statuses);
                    end Query;
                or
                    accept Quit do
                        Server_Task.Kill;
                        for Index in Readers'Range loop
                            Readers (Index).Kill;
                        end loop;
                        exit;
                    end Quit;
                or
                    accept Kill do
                        Server_Task.Kill;
                        for Index in Readers'Range loop
                            Readers (Index).Kill;
                        end loop;
                        exit;
                    end Kill;
                end select;
            end loop;
        end Run_Host;

    begin
        select
            accept Host (Which : in Lobby.Lobby_Element) do
                Is_Host := True;
                Key_String := Which.Map_Name;
                Max_Players := Which.Max_Players;
            end Host;
        or
            accept Join (Which : in Lobby.Lobby_Element) do
                Is_Host := False;
                Key_String := Which.Address;
                Max_Players := Which.Max_Players;
            end Join;
        end select;

        if Is_Host = True then
            Run_Host (Key_String, Max_Players);
        else
            Run_Client (Key_String, Max_Players);
        end if;
    end VL_Game;
end VL;
