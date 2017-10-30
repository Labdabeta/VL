with Boards;
with Actions;
with Servers;
with GNAT.Sockets; use GNAT.Sockets;

package body Clients is

    task body Client_Reader is
        Server : Servers.Server;
        Sock : Socket_Type;
        Connection : Stream_Access;
        We : Natural;
        W, H, PC : Positive;
        Read_Selection : Selector;
        Client_Set : Socket_Set_Type;
        Empty_Set : Socket_Set_Type;
        Read_Status : Selector_Status;
        Read_Delay : constant Duration := 0.1; -- poll at 10Hz
    begin
        accept Create (
            Reference : in out Servers.Server;
            Socket : in GNAT.Sockets.Socket_Type;
            Us : in Natural) do
            Server := Reference;
            Sock := Socket;
            We := Us;
        end Create;

        Connection := Stream (Sock);

        Server.Get_Dimensions (W, H, PC);

        Empty (Client_Set);
        Empty (Empty_Set);
        Set (Client_Set, Sock);
        Create_Selector (Read_Selection);
        loop
            select
                accept Kill do
                    Server.Quit (We);
                    Shutdown_Socket (Sock);
                    return;
                end Kill;
            else
                Check_Selector (Read_Selection,
                    Client_Set, Empty_Set,
                    Read_Status, Read_Delay);
                if Read_Status = Completed then
                    declare
                        Message_Type : Character;
                    begin
                        Character'Read (Connection, Message_Type);
                        case Message_Type is
                            when 'X' =>
                                Server.Quit (We);
                            when 'C' =>
                                Server.Commit (
                                    Actions.Action_Array'Input (Connection),
                                    We);
                            when 'U' =>
                                Server.Uncommit (We);
                        end case;
                    end;
                end if;
        end loop;
        Close_Selector (Read_Selection);
    end Client_Reader;

    task body Client_Writer is
        Server : Servers.Server;
        Sock : Socket_Type;
        Connection : Stream_Access;
        Us : Natural;
        W, H, PC : Positive;

        function To_Char (Act : Player_Activity) return Character is begin
            case Act is
                when EMPTY => return "E";
                when ACTIVE => return "A";
                when COMMITTED => return "C";
                when DISCONNECTED => return "D";
                when QUIT => return "Q";
                when SELF => return "S";
            end Act;
        end To_Char;
    begin
        accept Create (
            Reference : in out Servers.Server;
            Socket : in GNAT.Sockets.Socket_Type;
            Us : out Natural) do
            Server := Reference;
            Sock := Socket;
            Server.Connect (Us);
        end Create;

        Connection := Stream (Sock);

        Server.Get_Dimensions (W, H, PC);

        declare
            Board : Boards.Board (W, H);
            Statuses : Player_Status_Array (1 .. PC);
        begin
            loop
                Server.Query (Us, Board, Statuses);
                for Index in Statuses'Range loop
                    if Statuses (Index).Team = Us then
                        if Statuses (Index).Activity = QUIT or
                            Statuses (Index).Activity = DISCONNECTED
                        then
                            return;
                        end if;
                    end if;
                end loop;

                Boards.Board'Output (Connection, Board);
                for Index in Statuses'Range loop
                    Natural'Write (Connection, Statuses (Index).Team);
                    Character'Write (Connection,
                        To_Char (Statuses (Index).Activity));
                end loop;

                Server.Wait_For_Change;
            end loop;
        end;
    end Client_Writer;

end Clients;
