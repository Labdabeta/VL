package Coordinates is
    type Coordinate is
        record
            X, Y : Positive;
        end record;

    -- Calculates Chebyshev distance between source and destination
    function Distance (
        Source : in Coordinate;
        Destination : in Coordinate)
        return Natural;

    type Coordinate_List is array (Positive range <>) of Coordinates.Coordinate;

    function Get_Adjacent_Within (
        Source : in Coordinate;
        Bounds : in Coordinate)
        return Coordinate_List;
end Coordinates;
