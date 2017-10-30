with Servers;
with GNAT.Sockets;

package Clients is
    --  This merely writes to the socket the updates whenever they are available
    --  This kills itself when it realizes its player has quit or disconnected
    task type Client_Writer is
        entry Create (
            Reference : in out Servers.Server;
            Socket : in GNAT.Sockets.Socket_Type;
            Us : out Natural);
    end Client_Writer;

    --  This merely reads from the socket and updates the reference as
    --  applicable. Must be killable (marked with quit) in case the host quits.
    --  Uses Check_Selector on the socket with a timeout to allow this.
    --
    --  This will also shutdown the socket when killed.
    task type Client_Reader is
        entry Create (
            Reference : in out Servers.Server;
            Socket : in GNAT.Sockets.Socket_Type;
            Us : in Natural);
        entry Kill;
    end Client_Reader;
end Clients;
