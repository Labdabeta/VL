with Boards;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Deallocation;
with Paths;

package body Maps is

    function Create_Map (
        Name : String;
        Width : Positive;
        Height : Positive) return Map is
        New_Board : Boards.Board := Boards.Create_Board (Width, Height);
        Map_File : Ada.Text_IO.File_Type;
        Stream : Ada.Text_IO.Text_Streams.Stream_Access;
    begin
        declare begin
            Ada.Text_IO.Open (Map_File, Ada.Text_IO.Out_File,
                Paths.Map_Prefix & Name & ".vl");
        exception
            when Ada.Text_IO.Name_Error =>
                Ada.Text_IO.Create (Map_File, Ada.Text_IO.Out_File,
                Paths.Map_Prefix & Name & ".vl");
        end;
        Stream := Ada.Text_IO.Text_Streams.Stream (Map_File);
        Boards.Board'Output (Stream, New_Board);
        Ada.Text_IO.Close (Map_File);

        Ada.Text_IO.Open (Map_File, Ada.Text_IO.Append_File, Paths.List_File);
        Ada.Text_IO.Put_Line (Map_File, Name);
        Ada.Text_IO.Close (Map_File);

        return Load_Maps;
    end Create_Map;

    procedure Free_Maps (Which : in out Map) is
    begin
        if Which.Prev = null then
            return;
        end if;
        declare
            First : Map_Ptr := Which.Prev.Next;
            Current : Map_Ptr := First.Next;
            Next : Map_Ptr;
            procedure Free_Map is new Ada.Unchecked_Deallocation (
                Map, Map_Ptr);
        begin
            while Current /= First loop
                Next := Current.Next;
                Boards.Free_Board (Current.Contents);
                Free_Map (Current);
                Current := Next;
            end loop;

            Boards.Free_Board (Current.Contents);
            Free_Map (Current);
        end;
    end Free_Maps;

    function Load_Maps return Map is
        Result : Map_Ptr := null;
        List_File : Ada.Text_IO.File_Type;
    begin
        Ada.Text_IO.Open (List_File, Ada.Text_IO.In_File, Paths.List_File);

        while not Ada.Text_IO.End_Of_File (List_File) loop
            declare
                Document : String := Ada.Text_IO.Get_Line (List_File);
                Map_File : Ada.Text_IO.File_Type;
                Stream : Ada.Text_IO.Text_Streams.Stream_Access;
            begin
                Ada.Text_IO.Open (Map_File, Ada.Text_IO.In_File,
                    Paths.Map_Prefix & Document & ".vl");
                Stream := Ada.Text_IO.Text_Streams.Stream (Map_File);
                declare
                    Board : Boards.Board := Boards.Board'Input (Stream);
                begin
                    if Result = null then
                        Result := new Map;
                        Result.Contents := new Boards.Board (
                            Board.Width, Board.Height);
                        Result.Contents.all := Board;
                        if Document'Length > 120 then
                            Result.Name := Document (1 .. 120);
                        else
                            Result.Name (1 .. Document'Length) := Document;
                        end if;
                        Result.Name_Length := Document'Length;
                        Result.Prev := Result;
                        Result.Next := Result;
                    else
                        declare
                            New_Result : Map_Ptr := new Map;
                        begin
                            New_Result.Contents := new Boards.Board (
                                Board.Width, Board.Height);
                            New_Result.Contents.all := Board;
                            if Document'Length > 120 then
                                New_Result.Name := Document (1 .. 120);
                            else
                                New_Result.Name (1 .. Document'Length) :=
                                    Document;
                            end if;
                            New_Result.Name_Length := Document'Length;
                            New_Result.Prev := Result;
                            New_Result.Next := Result.Next;
                            Result.Next.Prev := New_Result;
                            Result.Next := New_Result;
                            if Result.Prev = Result then
                                Result.Prev := New_Result;
                            end if;
                            Result := New_Result;
                        end;
                    end if;
                end;
                Ada.Text_IO.Close (Map_File);
            end;
        end loop;

        Ada.Text_IO.Close (List_File);

        if Result /= null then
            return Result.all;
        else
            return (null, (others => ' '), 0, null, null);
        end if;
    end Load_Maps;

    procedure Next (This : in out Map) is
    begin
        This := This.Next.all;
    end Next;

    procedure Prev (This : in out Map) is
    begin
        This := This.Prev.all;
    end Prev;

    procedure Save (This : in Map) is
        Map_File : Ada.Text_IO.File_Type;
        Stream : Ada.Text_IO.Text_Streams.Stream_Access;
    begin
        Ada.Text_IO.Open (Map_File, Ada.Text_IO.Out_File,
            Paths.Map_Prefix & This.Name (1 .. This.Name_Length) & ".vl");
        Stream := Ada.Text_IO.Text_Streams.Stream (Map_File);
        Boards.Board'Output (Stream, This.Contents.all);
        Ada.Text_IO.Close (Map_File);
    end Save;
end Maps;
