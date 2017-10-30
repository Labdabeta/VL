package Lobby is
    type Lobby_Element is
        record
            Name : String (1 .. 120);
            Map_Name : String (1 .. 120);
            Address : String (1 .. 120);
            Max_Players : Positive;
        end record;

    type Lobby_Element_Array is array (Positive range <>) of Lobby_Element;
    type Lobby_Element_Array_Ptr is access Lobby_Element_Array;

    procedure Free (What : in out Lobby_Element_Array_Ptr);

    function Query_Lobby return Lobby_Element_Array;

    function Post_Lobby (Element : Lobby_Element)
        return Positive;

    procedure Remove_Post (Which : Positive);
end Lobby;
