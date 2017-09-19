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

end Coordinates;
