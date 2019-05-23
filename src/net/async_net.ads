with GNAT.Sockets;
with Ada.Streams;

-- Usage example:
--
-- READING
-- ~~~~~~~
-- My_Type'Read (Sock, Item);
-- while not Is_Ready (Sock) loop
--     Put_Line ("Waiting...");
-- end loop;
-- My_Type'Read (Sock, Item); -- To actually populate Item
-- Put (Item);
-- TODO: Make it 1-off instead?
--
-- WRITING
-- ~~~~~~~
-- My_Type'Write (Sock, Item);
-- while not Is_Ready (Sock) loop
--     Put_Line ("Waiting...");
-- end loop;
package Async_Net is
    type Socket is new Ada.Streams.Root_Stream_Type with private;
    type Socket_Access is access all Ada.Streams.Root_Stream_Type'Class;

    type Error_Type is (
        NO_ERROR, CONNECTION_ERROR, TIMEOUT_ERROR, USAGE_ERROR, UNKNOWN_ERROR);
    function Has_Error (Which : in Socket) return Boolean;
    function Get_Error (Which : in out Socket) return Error_Type;

    -- Creates a server-type socket
    procedure Create_Server (
        Port : in GNAT.Sockets.Port_Type;
        Result : out Socket;
        Timeout : in Duration := 0.1);

    -- Accepts a new client on the server
    procedure Accept_Server (
        Which : in Socket;
        Client_Address : out GNAT.Sockets.Sock_Addr_Type;
        Result : out Socket;
        Timeout : in Duration := 0.1);

    -- Creates a new client
    procedure Create_Client (
        Address : in String;
        Port : in GNAT.Sockets.Port_Type;
        Result : out Socket;
        Timeout : in Duration := 0.1);

    procedure Close_Socket (Which : in out Socket);

    function Is_Ready (Which : in out Socket) return Boolean;

    procedure Set_Timeout (Which : in out Socket; To : in Duration := 0.1);

    procedure Read (
        Stream : in out Socket;
        Item : out Ada.Streams.Stream_Element_Array;
        Last : out Ada.Streams.Stream_Element_Offset);

    procedure Write (
        Stream : in out Socket;
        Item : in Ada.Streams.Stream_Element_Array);
private
    type Stream_Element_Array_Access is access all
        Ada.Streams.Stream_Element_Array;
    type Socket_Status is (SS_IDLE, SS_READING, SS_WRITING, SS_CONNECTING);
    type Socket is new Ada.Streams.Root_Stream_Type with
        record
            -- Set as BLOCKING, then used timed selectors to prevent lock
            Sock : GNAT.Sockets.Socket_Type;
            Error : Error_Type;
            Timeout : Duration;
            State : Socket_Status;

            -- When item is null:
            --  Allocate it, and try to fill it
            -- When item is not null:
            --  Use it to return values, then deallocate it
            Item : Stream_Element_Array_Access;
        end record;
end Async_Net;
