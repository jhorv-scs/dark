#light

namespace dark
    open System

    type Terrain =
        | Floor
        | Wall
        | RockWall
        | Door
        | SecretDoor

    [<Flags>]
    type Flags = 
        | None = 0
        | Open = 1
        | Closed = 2
        

    type Tile = {
            x : int;
            y : int;
            Terrain : Terrain
            Flags : Flags}

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]            
    module Tile =        
        let get_display_char tile =        
            match tile.Terrain with
            | Floor      -> '.'
            | Wall       -> '#'
            | RockWall   -> ' '
            | Door       -> match tile.Flags with
                            | x when (x &&& Flags.Open) = Flags.Open -> '.'
                            | _                                      -> '+'
            | SecretDoor -> match tile.Flags with
                            | x when (x &&& Flags.Open) = Flags.Open -> '.'
                            | _                                      -> ' '
        
        let get_flags tile =
            tile.Flags
            
        let get_terrian tile = 
            tile.Terrain
        
        let get_x tile =
            tile.x
            
        let get_y tile = 
            tile.y

        let get_tile x y tiles =
            List.first (function | t when (get_x t) = x && (get_y t) = y -> Some(t) | _ -> None) tiles

        let get_door x y =
            {x = x; y = y; Terrain = Door; Flags = Flags.Closed}
            
        let get_door_secret x y = 
            {x = x; y = y; Terrain = SecretDoor; Flags = Flags.Closed}
            
        let get_floor x y =
            {x = x; y = y; Terrain = Floor; Flags = Flags.None}
            
        let get_rock x y = 
            {x = x; y = y; Terrain = RockWall; Flags = Flags.None}
            
        let get_wall x y =
            {x = x; y = y; Terrain = Wall; Flags = Flags.None}        

        let is_floor tile = 
            (get_terrian tile) = Floor
            
        let is_seeable tile = 
            match tile.Terrain with
            | Floor      -> true
            | Wall       -> false
            | RockWall   -> false
            | Door       -> match tile.Flags with
                            | x when (x &&& Flags.Open) = Flags.Open -> true
                            | _                                      -> false
            | SecretDoor -> match tile.Flags with
                            | x when (x &&& Flags.Open) = Flags.Open -> true
                            | _                                      -> false
            
        let is_rock tile = 
            (get_terrian tile) = RockWall
            
        let is_walkable tile = 
            match tile.Terrain with
            | Floor      -> true
            | Wall       -> false
            | RockWall   -> false
            | Door       -> match tile.Flags with
                            | x when (x &&& Flags.Open) = Flags.Open -> true
                            | _                                      -> false
            | SecretDoor -> match tile.Flags with
                            | x when (x &&& Flags.Open) = Flags.Open -> true
                            | _                                      -> false
            
        let is_wall tile = 
            (get_terrian tile) = Wall