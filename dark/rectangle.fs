#light

namespace dark
    open dark
    
    type Rectangle = { 
        X : int;
        Y : int;
        Width : int;
        Height : int;}

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]   
    module Rectangle =
        let get_x rect = 
            rect.X
            
        let get_y rect = 
            rect.Y
            
        let get_width rect =
            rect.Width
            
        let get_height rect = 
            rect.Height
            
        let get_left rect = 
            rect.X
            
        let get_right rect = 
            (get_x rect) + (get_width rect) - 1

        let get_top rect = 
            rect.Y
            
        let get_bottom rect = 
            (get_y rect) + (get_height rect) - 1
            
        let contains rect x y =
            (x >= (get_left rect)) && (x <= (get_right rect)) && (y >= (get_top rect)) && (y <= (get_bottom rect))
        
        let inflate rect width height =
            {X = (get_x rect) - width; Y = (get_y rect) - height; Width = (get_width rect) + (width * 2); Height = (get_height rect) + (height * 2);}
            
        let intersects_with rect1 rect2 =
            let points = [for x in (get_left rect1)..(get_right rect1) do
                            for y in (get_top rect1)..(get_bottom rect1) do
                                yield (x,y)]
            points |> List.exists (fun (x,y) -> contains rect2 x y) 