with GNAT.Sockets;
with Ada.Strings.Unbounded;

package body HTTP_Utils is
    function Char_To_Hex (What : Character) return Natural is
    begin
        case What is
            when '0' => return 0;
            when '1' => return 1;
            when '2' => return 2;
            when '3' => return 3;
            when '4' => return 4;
            when '5' => return 5;
            when '6' => return 6;
            when '7' => return 7;
            when '8' => return 8;
            when '9' => return 9;
            when 'a' | 'A' => return 10;
            when 'b' | 'B' => return 11;
            when 'c' | 'C' => return 12;
            when 'd' | 'D' => return 13;
            when 'e' | 'E' => return 14;
            when 'f' | 'F' => return 15;
            when others => return 0;
        end case;
    end Char_To_Hex;

    function Extract_Request (Data : GNAT.Sockets.Stream_Access)
        return String_List is
        Query : Character;
        Line_Length : Natural;
        Empty_List : String_List (1 .. 0);

        function Line_Split (
            Text : String;
            Current : String_List)
            return String_List is
            Sub_Length : Natural := Text'First;
            Next_Line : String_List (1 .. 1);
        begin
            while Sub_Length < Text'Last loop
                if Text (Sub_Length) = ASCII.LF then
                    Next_Line (1) := Ada.Strings.Unbounded.To_Unbounded_String (
                        Text (Text'First .. Sub_Length - 1));
                    return Line_Split (
                        Text (Sub_Length + 1 .. Text'Last),
                        Current & Next_Line);
                end if;
                Sub_Length := Sub_Length + 1;
            end loop;

            return Current;
        end Line_Split;

        function Read_Chunks return Ada.Strings.Unbounded.Unbounded_String is
            Chunk_Length : Natural := 0;
            Current : Ada.Strings.Unbounded.Unbounded_String :=
                Ada.Strings.Unbounded.To_Unbounded_String ("");
        begin
            loop
                loop
                    Character'Read (Data, Query);
                    exit when Query = ASCII.CR;
                    Chunk_Length := Chunk_Length * 16;
                    Chunk_Length := Chunk_Length + Char_To_Hex (Query);
                end loop;
                Character'Read (Data, Query); --  Extract the LF

                exit when Chunk_Length = 0;

                --  Read the chunk itself
                declare
                    Current_Chunk : String (1 .. Chunk_Length);
                begin
                    String'Read (Data, Current_Chunk);
                    Ada.Strings.Unbounded.Append (Current, Current_Chunk);

                    --  Read the terminating CR LF
                    Character'Read (Data, Query);
                    Character'Read (Data, Query);
                    Chunk_Length := 0;
                end;
            end loop;

            return Current;
        end Read_Chunks;
    begin
        --  Read until empty line
        loop
            Line_Length := 0;
            while Character'Input (Data) /= ASCII.CR loop
                Line_Length := Line_Length + 1;
            end loop;
            Character'Read (Data, Query); --  Extract the LF
            exit when Line_Length = 0;
        end loop;

        return Line_Split (
            Ada.Strings.Unbounded.To_String (Read_Chunks), Empty_List);
    end Extract_Request;
end HTTP_Utils;
