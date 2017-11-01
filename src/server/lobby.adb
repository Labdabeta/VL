with GNAT.Sockets; use GNAT.Sockets;
with Ada.Containers.Vectors;

with Paths;

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with HTTP_Utils;

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

package body Lobby is
    function Webify (What : String) return String;

    package Lobby_Vectors is new Ada.Containers.Vectors (
        Element_Type => Lobby_Element, Index_Type => Positive);
    HTTP_Port : constant Port_Type := 80;
    Lobby_Address : constant String := "www.labprogramming.net";
    HTTP_NL : constant String := ASCII.CR & ASCII.LF;
    Query_String : constant String := "GET /vl HTTP/1.1" & HTTP_NL &
        "Host: www.labprogramming.net" & HTTP_NL & HTTP_NL;
    Post_Start : constant String := "POST /vl?";
    Delete_Post_Start : constant String := "POST /vldelete?";
    Post_End : constant String := " HTTP/1.1" & HTTP_NL &
        "Host: www.labprogramming.net" & HTTP_NL & HTTP_NL;

    procedure Free (What : in out Lobby_Element_Array_Ptr) is
        procedure Free_LEA is new Ada.Unchecked_Deallocation (
            Lobby_Element_Array, Lobby_Element_Array_Ptr);
    begin
        Free_LEA (What);
    end Free;

    function Post_Lobby (Element : Lobby_Element)
        return Positive is
        Address : Sock_Addr_Type;
        Result : Socket_Type;
        Player_Count_String : String := Positive'Image (Element.Max_Players);
        PCS : String := Player_Count_String (
            Player_Count_String'First + 1 .. Player_Count_String'Last);
    begin
        Address.Addr := Addresses (Get_Host_By_Name (Lobby_Address), 1);
        Address.Port := HTTP_Port;
        Create_Socket (Result);
        Connect_Socket (Result, Address);
        String'Write (Stream (Result), Post_Start &
            "name=" & Webify (Element.Name) &
            "&map_name=" & Webify (Element.Map_Name) &
            "&max_players=" & PCS & Post_End);

        declare
            Request_Body : HTTP_Utils.String_List :=
                HTTP_Utils.Extract_Request (Stream (Result));
        begin
            Shutdown_Socket (Result);

            return Positive'Value (
                Ada.Strings.Unbounded.To_String (
                    Request_Body (Request_Body'First)));
        end;
    end Post_Lobby;

    function Query_Lobby return Lobby_Element_Array is
        Address : Sock_Addr_Type;
        Socket : Socket_Type;
        Elements : Lobby_Vectors.Vector := Lobby_Vectors.Empty_Vector;
        Data : Stream_Access;
        Started : Boolean := False;

        procedure Process_Line (What : String) is
        begin
            if Started then
                declare
                    New_Element : Lobby_Element := (
                        What (What'First .. What'First + 119),
                        What (What'First + 120 .. What'First + 239),
                        What (What'First + 240 .. What'First + 359),
                        Positive'Value (
                            What (What'First + 360 .. What'First + 369)));
                begin
                    Lobby_Vectors.Append (Elements, New_Element);
                end;
            end if;

            if What'Length = 5 and then (What = "START" and not Started) then
                Started := True;
            end if;
        end Process_Line;
    begin
        Address.Addr := Addresses (Get_Host_By_Name (Lobby_Address), 1);
        Address.Port := 80;
        Create_Socket (Socket);
        Connect_Socket (Socket, Address);
        Data := Stream (Socket);
        String'Write (Data, Query_String);

        declare
            Request_Body : HTTP_Utils.String_List :=
                HTTP_Utils.Extract_Request (Data);
        begin
            Shutdown_Socket (Socket);
            for Index in Request_Body'Range loop
                Process_Line (Ada.Strings.Unbounded.To_String (
                    Request_Body (Index)));
            end loop;
        end;

        declare
            Result : Lobby_Element_Array (1 .. Integer (Elements.Length));
        begin
            for I in Result'Range loop
                Result (I) := Elements.Element (I);
            end loop;
            return Result;
        end;
    end Query_Lobby;

    function Read_Board (Which : Lobby_Element) return Boards.Board is
        File : Ada.Text_IO.File_Type;
        Real_Length : Positive := 1;
    begin
        while Which.Map_Name (Real_Length) /= ' ' and
            Real_Length < Which.Map_Name'Last
        loop
            Real_Length := Real_Length + 1;
        end loop;
        Ada.Text_IO.Open (File, Ada.Text_IO.In_File,
            Paths.Map_Prefix & Which.Map_Name (1 .. Real_Length - 1) & ".vl");
        declare
            Result : Boards.Board := Boards.Board'Input (
                Ada.Text_IO.Text_Streams.Stream (File));
        begin
            Ada.Text_IO.Close (File);
            return Result;
        end;
    end Read_Board;

    procedure Remove_Post (Which : Positive) is
        Address : Sock_Addr_Type;
        Result : Socket_Type;
    begin
        Address.Addr := Addresses (Get_Host_By_Name (Lobby_Address), 1);
        Address.Port := HTTP_Port;
        Create_Socket (Result);
        Connect_Socket (Result, Address);
        String'Write (Stream (Result), Delete_Post_Start &
            "id=" & Positive'Image (Which) & Post_End);
        Shutdown_Socket (Result);
    end Remove_Post;

    function Webify (What : String) return String is
        --  At worst each character becomes %XX: a 3-fold increase
        Result : String (1 .. 3 * What'Length);
        Index : Natural := 0;
        Hex_Indexes : String (1 .. 16) := "0123456789ABCDEF";
    begin
        for I in What'Range loop
            if
                (What (I) <= 'Z' and What (I) >= 'A') or
                (What (I) <= 'z' and What (I) >= 'a') or
                (What (I) <= '9' and What (I) >= '0')
            then
                Index := Index + 1;
                Result (Index) := What (I);
            elsif What (I) = ' ' then
                Index := Index + 1;
                Result (Index) := '+';
            else
                Result (Index + 1) := '%';
                Result (Index + 2) := Hex_Indexes (
                    1 + (Character'Pos (What (I)) / 16));
                Result (Index + 3) := Hex_Indexes (
                    1 + (Character'Pos (What (I)) mod 16));
                Index := Index + 3;
            end if;
        end loop;

        return Result (1 .. Index);
    end Webify;
end Lobby;
