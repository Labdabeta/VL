package body Coordinates is

    function Distance (
        Source : in Coordinate;
        Destination : in Coordinate)
        return Natural is
        Dx : Integer := abs (Source.X - Destination.X);
        Dy : Integer := abs (Source.Y - Destination.Y);
    begin
        if Dx > Dy then
            return Dx;
        else
            return Dy;
        end if;
    end Distance;

    function Get_Adjacent_Within (
        Source : in Coordinate;
        Bounds : in Coordinate)
        return Coordinate_List is
        Results : Coordinate_List (1 .. 8);
        Result_Size : Integer := 1;
    begin
        if Source.X > 1 then
            Results (Result_Size) := (Source.X - 1, Source.Y);
            Result_Size := Result_Size + 1;
            if Source.Y > 1 then
                Results (Result_Size) := (Source.X - 1, Source.Y - 1);
                Result_Size := Result_Size + 1;
            end if;
            if Source.Y < Bounds.Y then
                Results (Result_Size) := (Source.X - 1, Source.Y + 1);
                Result_Size := Result_Size + 1;
            end if;
        end if;

        if Source.X < Bounds.X then
            Results (Result_Size) := (Source.X + 1, Source.Y);
            Result_Size := Result_Size + 1;
            if Source.Y > 1 then
                Results (Result_Size) := (Source.X + 1, Source.Y - 1);
                Result_Size := Result_Size + 1;
            end if;
            if Source.Y < Bounds.Y then
                Results (Result_Size) := (Source.X + 1, Source.Y + 1);
                Result_Size := Result_Size + 1;
            end if;
        end if;

        if Source.Y > 1 then
            Results (Result_Size) := (Source.X, Source.Y - 1);
            Result_Size := Result_Size + 1;
        end if;
        if Source.Y < Bounds.Y then
            Results (Result_Size) := (Source.X, Source.Y + 1);
            Result_Size := Result_Size + 1;
        end if;

        return Results (1 .. Result_Size - 1);
    end Get_Adjacent_Within;

end Coordinates;
