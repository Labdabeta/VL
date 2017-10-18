with Units; use Units;
with Ada.Streams; use Ada.Streams;

package body Tiles is

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Tile is
        Result : Tile;
        Buffer : Stream_Element_Array (1 .. 1);
        Last : Stream_Element_Offset;
    begin
        Stream.Read (Buffer, Last);
        Result.Kind := Tile_Kind'Val (Buffer (1) / 16);
        Result.Occupant.Unit := Occupant'Val (Buffer (1) mod 16);

        Stream.Read (Buffer, Last);
        Result.Occupant.Team := Natural (Buffer (1));

        return Result;
    end Input;

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Tile) is
        Buffer : Stream_Element_Array (1 .. 1);
    begin
        Buffer (1) := Stream_Element (
            Tile_Kind'Pos (Item.Kind) * 16 + Occupant'Pos (Item.Occupant.Unit));
        Stream.Write (Buffer);

        Buffer (1) := Stream_Element (Item.Occupant.Team);
        Stream.Write (Buffer);
    end Output;

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Tile) is
    begin
        --  Tiles don't have descriminants
        Item := Tile'Input (Stream);
    end Read;

    procedure Resolve (
        This : in out Tile;
        Presence : in Presence_Array;
        Create : in Boolean) is

        Matchup : array (Boolean, -- VAMPIRE
                         Boolean, -- LEPRECHAUN
                         Boolean, -- HUMAN
                         Boolean, -- FAIRY
                         Boolean) -- ZOMBIE
                         of Occupant := (
            True => ( -- V
                True => ( -- L
                    True => ( -- H
                        True => ( -- F
                            True => NONE,           -- VLHFZ
                            False => NONE),         -- VLHF
                        False => ( -- no F
                            True => NONE,           -- VLHZ
                            False => NONE)),        -- VLH
                    False => ( -- no H
                        True => ( -- F
                            True => NONE,           -- VLFZ
                            False => FAIRY),        -- VLF
                        False => ( -- no F
                            True => LEPRECHAUN,     -- VLZ
                            False => LEPRECHAUN))), -- VL
                False => ( -- no L
                    True => ( -- H
                        True => ( -- F
                            True => NONE,           -- VHFZ
                            False => NONE),         -- VHF
                        False => ( -- no F
                            True => VAMPIRE,        -- VHZ
                            False => VAMPIRE)),     -- VH
                    False => ( -- no H
                        True => ( -- F
                            True => NONE,           -- VFZ
                            False => FAIRY),        -- VF
                        False => ( -- no F
                            True => VAMPIRE,        -- VZ
                            False => VAMPIRE)))),   -- V
            False => ( -- no V
                True => ( -- L
                    True => ( -- H
                        True => ( -- F
                            True => NONE,           -- LHFZ
                            False => HUMAN),        -- LHF
                        False => ( -- no F
                            True => NONE,           -- LHZ
                            False => HUMAN)),       -- LH
                    False => ( -- no H
                        True => ( -- F
                            True => NONE,           -- LFZ
                            False => FAIRY),        -- LF
                        False => ( -- no F
                            True => LEPRECHAUN,     -- LZ
                            False => LEPRECHAUN))), -- L
                False => ( -- no L
                    True => ( -- H
                        True => ( -- F
                            True => ZOMBIE,         -- HFZ
                            False => HUMAN),        -- HF
                        False => ( -- no F
                            True => ZOMBIE,         -- HZ
                            False => HUMAN)),       -- H
                    False => ( -- no H
                        True => ( -- F
                            True => ZOMBIE,         -- FZ
                            False => FAIRY),        -- F
                        False => ( -- no F
                            True => ZOMBIE,         -- Z
                            False => NONE)))));     -- NONE

        type Attendance_Array is array (Occupant) of Boolean;

        function Check_Matchup (Attendance : in Attendance_Array)
            return Occupant is
        begin
            return Matchup (Attendance (VAMPIRE),
                            Attendance (LEPRECHAUN),
                            Attendance (HUMAN),
                            Attendance (FAIRY),
                            Attendance (ZOMBIE));
        end Check_Matchup;

        Attendance : Attendance_Array := (others => False);
        Duplicated : Attendance_Array := (NONE => True, others => False);
        Check_Unit : Occupant;
        Victor : Occupant := NONE;
    begin

        -- Check for creation
        if Create and This.Kind = PIT then
            This.Kind := FLOOR;
        end if;

        if This.Occupant.Unit /= NONE then
            Attendance (This.Occupant.Unit) := True;
        end if;

        for Item in Presence'Range loop
            Check_Unit := Presence (Item).Unit;
            if Attendance (Check_Unit) then
                Duplicated (Check_Unit) := True;
            end if;
            Attendance (Check_Unit) := True;
        end loop;

        Victor := Check_Matchup (Attendance);

        if Duplicated (Victor) then
            This.Occupant.Unit := NONE;
            if This.Kind /= BASE or Victor /= NONE then
                This.Occupant.Team := 0;
            end if;
        else
            for Item in Presence'Range loop
                if Presence (Item).Unit = Victor then
                    This.Occupant := Presence (Item);
                    exit;
                end if;
            end loop;
        end if;

        if (This.Kind = PIT and This.Occupant.Unit /= FAIRY) or
            This.Kind = IMPASSABLE
        then
            This.Occupant := (
                Unit => NONE,
                Team => 0);
        end if;
    end Resolve;

    function To_String (
        This : in Tile)
        return String is
        Team_Image : String := Positive'Image (This.Occupant.Team);
    begin
        case This.Kind is
            when BASE =>
                return ASCII.ESC & "[46m" & To_String (This.Occupant.Unit) &
                    Team_Image (2 .. Team_Image'Last) &
                    ASCII.ESC & "[39m" & ASCII.ESC & "49m";
            when PIT =>
                return ASCII.ESC & "[40m" & To_String (This.Occupant.Unit) &
                    Team_Image (2 .. Team_Image'Last) &
                    ASCII.ESC & "[39m" & ASCII.ESC & "49m";
            when FLOOR =>
                return ASCII.ESC & "[47m" & To_String (This.Occupant.Unit) &
                    Team_Image (2 .. Team_Image'Last) &
                    ASCII.ESC & "[39m" & ASCII.ESC & "49m";
            when PRODUCER =>
                return ASCII.ESC & "[43m" & To_String (This.Occupant.Unit) &
                    Team_Image (2 .. Team_Image'Last) &
                    ASCII.ESC & "[39m" & ASCII.ESC & "49m";
            when IMPASSABLE =>
                return ASCII.ESC & "[41m" & To_String (This.Occupant.Unit) &
                    Team_Image (2 .. Team_Image'Last) &
                    ASCII.ESC & "[39m" & ASCII.ESC & "49m";
            when UNKNOWN =>
                return ASCII.ESC & "[44m" & To_String (This.Occupant.Unit) &
                    Team_Image (2 .. Team_Image'Last) &
                    ASCII.ESC & "[39m" & ASCII.ESC & "49m";
        end case;
    end To_String;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Tile) is
    begin
        --  Tiles don't have descriminants
        Tile'Output (Stream, Item);
    end Write;
end Tiles;
