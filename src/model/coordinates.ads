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
end Coordinates;
