with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Unchecked_Deallocation;

package body Async_Net is
    procedure Free_SEA is new Ada.Unchecked_Deallocation (
        Ada.Streams.Stream_Element_Array, Stream_Element_Array_Access);
    NBIOR : constant Request_Type := (Non_Blocking_IO, True);

    procedure Accept_Server (
        Which : in Socket;
        Client_Address : out GNAT.Sockets.Sock_Addr_Type;
        Result : out Socket;
        Timeout : in Duration := 0.1) is
        Res : Socket_Type;
        Accept_Selector : Selector_Type;
        Empty_Set : Socket_Set_Type;
        Socket_Set : Socket_Set_Type;
        Accept_Status : Selector_Status;
        Tmp : Request_Type := NBIOR;
    begin
        Empty (Empty_Set);
        Empty (Socket_Set);
        Set (Socket_Set, Which.Sock);
        Create_Selector (Accept_Selector);
        Check_Selector (Accept_Selector,
            Socket_Set, Empty_Set, Accept_Status, Which.Timeout);
        if Accept_Status = Completed then
            Accept_Socket (Which.Sock, Res, Client_Address);
            Control_Socket (Res, Tmp);
            Result.Sock := Res;
            Result.Error := NO_ERROR;
            Result.Timeout := Timeout;
            Result.State := SS_IDLE;
            Result.Item := null;
        else
            Result.Sock := Res;
            Result.Error := TIMEOUT_ERROR;
            Result.Timeout := Timeout;
            Result.State := SS_IDLE;
            Result.Item := null;
        end if;
    end Accept_Server;

    procedure Close_Socket (Which : in out Socket) is
    begin
        Shutdown_Socket (Which.Sock);
    end Close_Socket;

    procedure Create_Client (
        Address : in String;
        Port : in GNAT.Sockets.Port_Type;
        Result : out Socket;
        Timeout : in Duration := 0.1) is
        Res : Socket_Type;
        Sock_Address : Sock_Addr_Type;
        Tmp : Request_Type := NBIOR;
    begin
        Sock_Address.Addr := Addresses (Get_Host_By_Name (Address), 1);
        Sock_Address.Port := Port;
        Create_Socket (Res);
        Set_Socket_Option (Res, Socket_Level, (Reuse_Address, True));
        Control_Socket (Res, Tmp);
        declare begin
            Connect_Socket (Res, Sock_Address);
        exception
            when others => -- EINPROGRESS (because nonblocking)
                Result.State := SS_CONNECTING;
        end;
        Result.Sock := Res;
        Result.Error := NO_ERROR;
        Result.Timeout := Timeout;
        Result.Item := null;
        Result.State := SS_CONNECTING;
    end Create_Client;

    procedure Create_Server (
        Port : in GNAT.Sockets.Port_Type;
        Result : out Socket;
        Timeout : in Duration := 0.1) is
        Res : Socket_Type;
        Local_Address : Sock_Addr_Type;
        Tmp : Request_Type := NBIOR;
    begin
        Create_Socket (Res);
        Set_Socket_Option (Res, Socket_Level, (Reuse_Address, True));
        Control_Socket (Res, Tmp);

        Local_Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
        Local_Address.Port := Port;

        Bind_Socket (Res, Local_Address);
        Listen_Socket (Res);
        Result.Sock := Res;
        Result.Error := NO_ERROR;
        Result.Timeout := Timeout;
        Result.State := SS_IDLE;
        Result.Item := null;
    end Create_Server;

    function Get_Error (Which : in out Socket) return Error_Type is
        Result : Error_Type;
    begin
        Result := Which.Error;
        Which.Error := NO_ERROR;
        return Result;
    end Get_Error;

    function Has_Error (Which : in Socket) return Boolean is
    begin
        return Which.Error /= NO_ERROR;
    end Has_Error;

    function Is_Ready (Which : in out Socket) return Boolean is
        Selector : Selector_Type;
        Socket_Set : Socket_Set_Type;
        Empty_Set : Socket_Set_Type;
        Status : Selector_Status;
        Last : Ada.Streams.Stream_Element_Offset;
        procedure Prepare_Sets is
        begin
            Empty (Socket_Set);
            Empty (Empty_Set);
            Set (Socket_Set, Which.Sock);
            Create_Selector (Selector);
        end Prepare_Sets;

        procedure Truncate_Item is
            New_Array : Stream_Element_Array_Access :=
                new Stream_Element_Array (Last + 1 .. Which.Item'Last);
        begin
            New_Array.all := Which.Item (Last + 1 .. Which.Item'Last);
            Free_SEA (Which.Item);
            Which.Item := New_Array;
        end Truncate_Item;
    begin
        case Which.State is
            when SS_READING =>
                Prepare_Sets;
                Check_Selector (
                    Selector,
                    Socket_Set, -- Read
                    Empty_Set, -- Write
                    Status,
                    Which.Timeout);
                if Status = Completed then
                    Receive_Socket (Which.Sock, Which.Item.all, Last);
                    if Last = Which.Item'First - 1 then
                        Which.Error := CONNECTION_ERROR;
                        Which.State := SS_IDLE;
                    elsif Last = Which.Item'Last then
                        Which.State := SS_IDLE;
                    else
                        Truncate_Item;
                    end if;
                end if;
            when SS_WRITING =>
                Prepare_Sets;
                Check_Selector (
                    Selector,
                    Empty_Set, -- Read
                    Socket_Set, -- Write
                    Status,
                    Which.Timeout);
                if Status = Completed then
                    Send_Socket (Which.Sock, Which.Item.all, Last);
                    if Last = Which.Item'First then
                        Which.Error := CONNECTION_ERROR;
                        Which.State := SS_IDLE;
                    elsif Last = Which.Item'Last then
                        Which.State := SS_IDLE;
                    else
                        Truncate_Item;
                    end if;
                end if;
            when SS_CONNECTING =>
                Prepare_Sets;
                Check_Selector (
                    Selector,
                    Empty_Set, -- Read
                    Socket_Set, -- Write
                    Status,
                    Which.Timeout);
                if Status = Completed then
                    Which.State := SS_IDLE;
                end if;
            when others => null;
        end case;

        return Which.State = SS_IDLE;
    end Is_Ready;

    procedure Read (
        Stream : in out Socket;
        Item : out Ada.Streams.Stream_Element_Array;
        Last : out Ada.Streams.Stream_Element_Offset) is
    begin
        if Stream.Item = null then
            Stream.Item := new Ada.Streams.Stream_Element_Array (
                Item'First .. Item'Last);
            Stream.State := SS_READING;
        else
            for I in Item'Range loop
                Item (I) := Stream.Item (I);
            end loop;

            Last := Stream.Item'Last;

            Free_SEA (Stream.Item);
        end if;
    end Read;

    procedure Set_Timeout (Which : in out Socket; To : in Duration := 0.1) is
    begin
        Which.Timeout := To;
    end Set_Timeout;

    procedure Write (
        Stream : in out Socket;
        Item : in Ada.Streams.Stream_Element_Array) is
    begin
        if Stream.Item /= null then
            Stream.Error := USAGE_ERROR;
            return;
        end if;

        Stream.Item := new Ada.Streams.Stream_Element_Array (
            Item'First ..  Item'Last);
        Stream.Item.all := Item;
        Stream.State := SS_WRITING;
    end Write;
end Async_Net;
