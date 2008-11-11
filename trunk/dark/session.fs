#light

namespace dark
    open libtcodWrapper    
    open dark    
    
    type SessionStatus =
    | Continue
    | Terminate
    
    type Session = {
        Player : Character;
        CurrentMap : Map;
        World : World;
        RootConsole : RootConsole;
        Console : Console;
        Status : SessionStatus;}
        
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]   
    module Session =     
        let get_console session = 
            session.Console

        let get_current_map session =
            session.CurrentMap
            
        let get_player session = 
            session.Player
            
        let get_root_console session = 
            session.RootConsole
            
        let get_status session =
            session.Status
            
        let get_world session = 
            session.World
            
        let generate width height = 
            let x = RootConsole.GetInstance()
            let y = RootConsole.GetNewConsole(width, height)
            
            let s = {
                RootConsole = RootConsole.GetInstance();
                Console = RootConsole.GetNewConsole(width, height);
                Status = Continue;
                Player = {x = 0; y = 0; Display = '@'};
                CurrentMap = Map.Empty;
                World = {Map = (Map.generate width height)}}  
                //World = {Map = (Map.generate_debug width height (RootConsole.GetInstance()))}}  
            let m = get_world s |> World.get_map
            match (Map.get_empty_tile m) with
            | None -> failwith "No Empty Tiles to Start Player in"
            | Some(t) -> {s with CurrentMap = m; Player = {s.Player with x = (Tile.get_x t); y = (Tile.get_y t)}}               