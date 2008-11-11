#light

namespace dark
    open System
    open libtcodWrapper
    open dark
    
    type MapObject = {
        Tiles : Tile list;
        Height : int;
        Width : int;}

    type Map =
        | Empty
        | Map of MapObject

    module MapInternal = 
        type Room = {
            x : int;
            y : int;
            Height : int;
            Width : int;}
        
        type MapBuild = {
            Width : int;
            Height : int;
            Tiles : Tile list;
            Rooms : Rectangle list;
            Corridors : Rectangle list;
            Connectors : Rectangle list;}
                        
        type Direction = 
            | Up
            | Down
            | Left
            | Right
            
        let r = new Random()                
        
        let get_tile_helper x y tiles = 
            List.first (fun t -> if (Tile.get_x t) = x && (Tile.get_y t) = y then Some(t) else None) tiles                
            
        let map_test emptyValueF mapValueF map =
            match map with
            | Empty -> emptyValueF()
            | Map(x) -> (mapValueF x)     
            
        let convert_tile test_f convert_f tile = 
            match test_f tile with
            | true -> convert_f tile
            | false -> tile
            
        let tile_in_room (room:Rectangle) tile = 
            Rectangle.contains room (Tile.get_x tile) (Tile.get_y tile)
            

        let draw_map (console:RootConsole) tiles =
            List.iter (fun t -> console.PutChar(Tile.get_x t,Tile.get_y t,Tile.get_display_char t, new Background(BackgroundFlag.Set))) tiles                         
        
        let get_direction () =
            match r.Next(0,3) with
            | 0 -> Direction.Up
            | 1 -> Direction.Right
            | 2 -> Direction.Down
            | 3 -> Direction.Left
            | _ -> failwith "Not valid direction"
       
        let get_corridor_length () = 
            r.Next(3,20)
                                
        let get_corridor x y direction = 
            let length = get_corridor_length()
            match direction with
            | Up -> {X = x; Y = y - length - 1; Width = 1; Height = length}
            | Right -> {X = x + 2; Y = y; Width = length; Height = 1}
            | Down -> {X = x; Y = y + 2; Width = 1; Height = length}
            | Left -> {X = x - length - 1; Y = y; Width = length; Height = 1}
         
        let get_corridor_end mapBuild = 
            let corridor = mapBuild.Corridors.[r.Next(0, List.length mapBuild.Corridors)]
            match corridor.Width, corridor.Height, r.Next(0,2) with
            | 1,_,0 -> match r.Next(0,3) with
                       | 0 -> (Rectangle.get_left corridor, Rectangle.get_top corridor, (Rectangle.get_left corridor) - 1, Rectangle.get_top corridor, Left)
                       | 1 -> (Rectangle.get_left corridor, Rectangle.get_top corridor, Rectangle.get_left corridor, (Rectangle.get_top corridor) - 1, Up)
                       | 2 -> (Rectangle.get_right corridor, Rectangle.get_top corridor, (Rectangle.get_right corridor) + 1, Rectangle.get_top corridor, Right)
            | 1,_,1 -> match r.Next(0,3) with
                       | 0 -> (Rectangle.get_left corridor, Rectangle.get_bottom corridor, (Rectangle.get_left corridor) - 1, Rectangle.get_bottom corridor, Left)
                       | 1 -> (Rectangle.get_right corridor, Rectangle.get_bottom corridor, (Rectangle.get_right corridor) + 1, Rectangle.get_bottom corridor, Right)
                       | 2 -> (Rectangle.get_left corridor, Rectangle.get_bottom corridor, Rectangle.get_left corridor, (Rectangle.get_bottom corridor) + 1, Down)
            | _,1,0 -> match r.Next(0,3) with
                       | 0 -> (Rectangle.get_left corridor, Rectangle.get_top corridor, (Rectangle.get_left corridor) - 1, Rectangle.get_top corridor, Left)
                       | 1 -> (Rectangle.get_left corridor, Rectangle.get_top corridor, Rectangle.get_left corridor, (Rectangle.get_top corridor) - 1, Up)
                       | 2 -> (Rectangle.get_left corridor, Rectangle.get_top corridor, Rectangle.get_left corridor, (Rectangle.get_bottom corridor) + 1, Down)
            | _,1,1 -> match r.Next(0,3) with
                       | 0 -> (Rectangle.get_right corridor, Rectangle.get_top corridor, Rectangle.get_right corridor, (Rectangle.get_top corridor) - 1, Up)
                       | 1 -> (Rectangle.get_right corridor, Rectangle.get_top corridor, (Rectangle.get_right corridor) + 1, Rectangle.get_top corridor, Right)
                       | 2 -> (Rectangle.get_right corridor, Rectangle.get_top corridor, Rectangle.get_right corridor, (Rectangle.get_bottom corridor) + 1, Down)           
        
        let get_corridor_point mapBuild = 
            let corridor = mapBuild.Corridors.[r.Next(0, List.length mapBuild.Corridors)]
            let x,y,direction = (r.Next(Rectangle.get_left corridor, Rectangle.get_right corridor), r.Next(Rectangle.get_top corridor, Rectangle.get_bottom corridor), get_direction())
            
            match direction with
            | Up    -> (x,y,x,y - 1,direction)
            | Right -> (x,y,x + 1,y,direction)
            | Down  -> (x,y,x,y + 1,direction)
            | Left  -> (x,y,x - 1,y,direction)
        
        let get_room x y direction = 
            let width = r.Next(3,6)
            let height = r.Next(3,6)
            
            match direction with
            | Up -> {X = x - r.Next(1,width); Y = y - 1 - height; Width = width; Height = height}
            | Right -> {X = x + 2; Y = y - r.Next(1, height); Width = width; Height = height}
            | Down -> {X = x - r.Next(1, width); Y = y + 2; Width = width; Height = height}
            | Left -> {X = x - 1 - width; Y = y - r.Next(1, height); Width = width; Height = height}
        
        let get_room_edge mapBuild =
            let room = mapBuild.Rooms.[r.Next(0, List.length mapBuild.Rooms)]
            let direction = get_direction()
            
            let x,y = match direction with
                      | Up -> (r.Next(Rectangle.get_left room, Rectangle.get_right room), Rectangle.get_top room)
                      | Right -> (Rectangle.get_right room, r.Next(Rectangle.get_top room, Rectangle.get_bottom room))            
                      | Down -> (r.Next(Rectangle.get_left room, Rectangle.get_right room), Rectangle.get_bottom room)
                      | Left -> (Rectangle.get_left room, r.Next(Rectangle.get_top room, Rectangle.get_bottom room))            
                      
            match direction with
            | Up    -> (x,y,x,y - 1,direction)
            | Right -> (x,y,x + 1,y,direction)
            | Down  -> (x,y,x,y + 1,direction)
            | Left  -> (x,y,x - 1,y,direction)

        let test_room mapBuild (room:Rectangle) = 
            let testRoom = Rectangle.inflate room 1 1 

            let testerFun = (List.exists (fun r -> Rectangle.intersects_with testRoom r))
            
            ((Rectangle.get_left room) > 1) 
            && ((Rectangle.get_top room) > 1) 
            && ((Rectangle.get_right room) < mapBuild.Width) 
            && ((Rectangle.get_bottom  room) < mapBuild.Height) 
            && not(testerFun mapBuild.Rooms) 
            && not(testerFun mapBuild.Corridors)
            && not(testerFun mapBuild.Connectors)

        let make_connector type_f x y tiles =
            let process_fun = convert_tile (fun t -> (Tile.get_x t = x) && (Tile.get_y t = y)) (fun t -> type_f (Tile.get_x t) (Tile.get_y t))
            List.map (convert_tile (fun t -> (Tile.get_x t = x) && (Tile.get_y t = y)) (fun t -> type_f (Tile.get_x t) (Tile.get_y t))) tiles

        let make_next getPointFun getRoomFun testRoomFun tileTestFun incrementorFun mapBuild =
            let x,y,doorX,doorY,direction = getPointFun mapBuild
            let room = getRoomFun x y direction
            if (testRoomFun mapBuild room) then
                {mapBuild with Corridors = ({X = doorX; Y = doorY; Width = 1; Height = 1} :: mapBuild.Corridors)} |> incrementorFun room
            else
                mapBuild

        let rec generate_loop mapBuild = 
            let corridorIncr = (fun r mb -> {mb with Corridors = (r :: mb.Corridors)})
            let roomIncr = (fun r mb -> {mb with Rooms = (r :: mb.Rooms)})
            let connectorIncr = (fun r mb -> {mb with Connectors = ( r :: mb.Connectors)})
            match (List.length mapBuild.Rooms) with
            | 20 -> mapBuild
            | _  -> match r.Next(0,100) with
                    | x when x < 16  -> generate_loop (make_next get_corridor_point get_corridor test_room tile_in_room corridorIncr mapBuild)
                    | x when x < 32  -> generate_loop (make_next get_corridor_end get_corridor test_room tile_in_room corridorIncr mapBuild)
                    | x when x < 48  -> generate_loop (make_next get_room_edge get_corridor test_room tile_in_room corridorIncr mapBuild)
                    | x when x < 64  -> generate_loop (make_next get_corridor_point get_room test_room tile_in_room roomIncr mapBuild)
                    | x when x < 80  -> generate_loop (make_next get_corridor_end get_room test_room tile_in_room roomIncr mapBuild)
                    | x when x < 100 -> generate_loop (make_next get_room_edge get_room test_room tile_in_room roomIncr mapBuild)
                    | _ -> failwith "Shouldn't be here"  
          
        let rec generate_loop_debug (console:RootConsole) mapBuild = 
            console.Clear()
            draw_map console mapBuild.Tiles
            console.Flush()
            
            Keyboard.WaitForKeyPress(true) |> ignore            
            
            let corridorIncr = (fun r mb -> {mb with Corridors = (r :: mb.Corridors)})
            let roomIncr = (fun r mb -> {mb with Rooms = (r :: mb.Rooms)})
            match (List.length mapBuild.Rooms) with
            | 20 -> mapBuild
            | _  -> match r.Next(0,100) with
                    | x when x < 16  -> generate_loop_debug console (make_next get_corridor_point get_corridor test_room tile_in_room corridorIncr mapBuild)
                    | x when x < 32  -> generate_loop_debug console (make_next get_corridor_end get_corridor test_room tile_in_room corridorIncr mapBuild)
                    | x when x < 48  -> generate_loop_debug console (make_next get_room_edge get_corridor test_room tile_in_room corridorIncr mapBuild)
                    | x when x < 64  -> generate_loop_debug console (make_next get_corridor_point get_room test_room tile_in_room roomIncr mapBuild)
                    | x when x < 80  -> generate_loop_debug console (make_next get_corridor_end get_room test_room tile_in_room roomIncr mapBuild)
                    | x when x < 100 -> generate_loop_debug console (make_next get_room_edge get_room test_room tile_in_room roomIncr mapBuild)
                    | _ -> failwith "Shouldn't be here"    

        let buildFirstRoom mapBuild =
            let room = {X = (mapBuild.Width / 2); Y = (mapBuild.Height / 2); Width = r.Next(3,6); Height = r.Next(3,6)}
            let process_fun = convert_tile (tile_in_room room) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles); Rooms = (room :: mapBuild.Rooms)}
        
        let is_endpoint_connected x y corridor mapBuild =
            let possiblePoints = Set.of_list ([for a in (x-1)..(x+1) do
                                                 for b in (y-1)..(y+1) do
                                                    yield (a,b)])
            let corridorPoints = Set.of_list (Rectangle.get_points corridor)
            let points = Set.diff possiblePoints corridorPoints
            let rooms = mapBuild.Rooms @ mapBuild.Corridors @ mapBuild.Connectors
            Set.exists (fun p -> List.exists (fun r -> Rectangle.contains r (fst p) (snd p)) rooms) points
                        
        let is_dead_end corridor mapBuild =
            let x1,y1 = (Rectangle.get_left corridor, Rectangle.get_top corridor)
            let x2,y2 = (Rectangle.get_right corridor, Rectangle.get_top corridor)
            let v = not(is_endpoint_connected x1 y1 corridor mapBuild) || not(is_endpoint_connected x2 y2 corridor mapBuild)
            v
            
        let trim_dead_ends mapBuild =
            {mapBuild with Corridors = (List.filter (fun c -> not (is_dead_end c mapBuild)) mapBuild.Corridors)}

        let dig_tiles mapBuild =
            let rooms = mapBuild.Rooms @ mapBuild.Corridors @ mapBuild.Connectors
            let test tile = List.exists (fun r -> Rectangle.contains r (Tile.get_x tile) (Tile.get_y tile)) rooms
            let tiles = mapBuild.Tiles |> List.map (convert_tile test (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t)))                   
            {mapBuild with Tiles = tiles}
                        
        let generate_blank width height =                
            { Width = width;
              Height = height;
              Tiles = [for row in 0 .. (height - 1) do
                        for column in 0 .. (width - 1) do
                            yield (Tile.get_rock column row) ];
              Rooms = [];
              Corridors = [];
              Connectors = [];}
              
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Map =   
        open dark
                                      
        let generate width height =                           
            let mb = MapInternal.generate_blank width height 
                     |> MapInternal.buildFirstRoom
                     |> MapInternal.make_next MapInternal.get_room_edge MapInternal.get_corridor MapInternal.test_room MapInternal.tile_in_room (fun r mb -> {mb with Corridors = (r :: mb.Corridors)})
                     |> MapInternal.make_next MapInternal.get_room_edge MapInternal.get_corridor MapInternal.test_room MapInternal.tile_in_room (fun r mb -> {mb with Corridors = (r :: mb.Corridors)})
                     |> MapInternal.generate_loop
                     |> MapInternal.trim_dead_ends
                     |> MapInternal.dig_tiles
            Map({Tiles = mb.Tiles; 
             Height = height;
             Width = width;})        
        
        let generate_debug width height console = 
            let mb = MapInternal.generate_blank width height 
                     |> MapInternal.buildFirstRoom 
                     |> MapInternal.make_next MapInternal.get_room_edge MapInternal.get_corridor MapInternal.test_room MapInternal.tile_in_room (fun r mb -> {mb with Corridors = (r :: mb.Corridors)})
                     |> MapInternal.make_next MapInternal.get_room_edge MapInternal.get_corridor MapInternal.test_room MapInternal.tile_in_room (fun r mb -> {mb with Corridors = (r :: mb.Corridors)})
                     |> MapInternal.generate_loop_debug console
                     |> MapInternal.trim_dead_ends
                     |> MapInternal.dig_tiles
            Map({Tiles = mb.Tiles; 
             Height = height;
             Width = width;})        
             
        let get_tiles map = 
            MapInternal.map_test (fun () -> []) (fun x -> x.Tiles) map

        let get_tile x y map = 
            MapInternal.get_tile_helper x y (get_tiles map)
            
        let get_empty_tile map = 
            List.first (fun t -> if (Tile.is_walkable t) then Some(t) else None) (get_tiles map)
                        
        let is_empty map = 
            MapInternal.map_test (fun () -> false) (fun x -> true) map
            
        
