with GNAT.Sockets;
with Ada.Strings.Unbounded;

package HTTP_Utils is
    type String_List is array (Positive range <>) of
        Ada.Strings.Unbounded.Unbounded_String;

    function Extract_Request (Data : GNAT.Sockets.Stream_Access)
        return String_List;
end HTTP_Utils;
