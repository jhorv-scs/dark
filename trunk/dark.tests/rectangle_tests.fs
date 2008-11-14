#light

namespace dark_tests 
    open FsUnit
    open dark
    
    module rectangle_tests =
        let tests () = 
            let rect = {X = 5; Y = 10; Width = 5; Height = 5}
            let smallRect = {X = 1; Y = 1; Width = 2; Height = 3}
            let rect2 = {X = 3; Y = 8; Width = 3; Height = 4}
            let rect3 = {X = 15; Y = 20; Width = 1; Height = 1}
            
            [
                spec "get_x should be 5" (Rectangle.get_x rect |> should equal 5)
                spec "get_y should be 10" (Rectangle.get_y rect |> should equal 10)
                spec "get_left should be 5" (Rectangle.get_left rect |> should equal 5)
                spec "get_right should be 9" (Rectangle.get_right rect |> should equal 9)
                spec "get_top should be 10" (Rectangle.get_top rect |> should equal 10)
                spec "get_bottom should be 14" (Rectangle.get_bottom rect |> should equal 14)
                spec "get_width should be 5" (Rectangle.get_width rect |> should equal 5)
                spec "get_height should be 5" (Rectangle.get_height rect |> should equal 5)
                spec "inflate 1 0 should return a rectangle with x of 4" (Rectangle.get_x (Rectangle.inflate rect 1 0) |> should equal 4)
                spec "inflate 1 0 should return a rectangle with a width of 7" (Rectangle.get_width (Rectangle.inflate rect 1 0) |> should equal 7)                                  
                spec "inflate 1 0 should return a rectangle with y of 10" (Rectangle.get_y (Rectangle.inflate rect 1 0) |> should equal 10)
                spec "inflate 1 0 should return a rectangle with a height of 5" (Rectangle.get_height (Rectangle.inflate rect 1 0) |> should equal 5)
                spec "inflate 0 1 should return a rectangle with y of 9" (Rectangle.get_y (Rectangle.inflate rect 0 1) |> should equal 9)
                spec "inflate 0 1 should return a rectangle with a height of 7" (Rectangle.get_height (Rectangle.inflate rect 0 1) |> should equal 7)
                spec "inflate 0 1 should return a rectangle with x of 5" (Rectangle.get_x (Rectangle.inflate rect 0 1) |> should equal 5)
                spec "inflate 0 1 should return a rectangle with a width of 5" (Rectangle.get_width (Rectangle.inflate rect 0 1) |> should equal 5)
                spec "inflate 1 1 should return a rectangle with y of 9" (Rectangle.get_y (Rectangle.inflate rect 1 1) |> should equal 9)
                spec "inflate 1 1 should return a rectangle with a height of 7" (Rectangle.get_height (Rectangle.inflate rect 1 1) |> should equal 7)
                spec "inflate 1 1 should return a rectangle with x of 4" (Rectangle.get_x (Rectangle.inflate rect 1 1) |> should equal 4)
                spec "inflate 1 1 should return a rectangle with a width of 7" (Rectangle.get_width (Rectangle.inflate rect 1 1) |> should equal 7)
                spec "contains 1 1 should be false" (Rectangle.contains rect 1 1 |> should be False)
                spec "contains 6 11 should be true" (Rectangle.contains rect 6 11 |> should be True)
                spec "get_points should contain (1,1)" (Rectangle.get_points smallRect |> should contain (1,1))
                spec "get_points should contain (1,2)" (Rectangle.get_points smallRect |> should contain (1,2))
                spec "get_points should contain (2,1)" (Rectangle.get_points smallRect |> should contain (2,1))
                spec "get_points should contain (2,2)" (Rectangle.get_points smallRect |> should contain (2,2))
                spec "get_points should contain (1,3)" (Rectangle.get_points smallRect |> should contain (1,3))
                spec "get_points should contain (2,3)" (Rectangle.get_points smallRect |> should contain (2,3))
                spec "intersects_with intersecting rectangle should be true" (Rectangle.intersects_with rect rect2 |> should be True)
                spec "intersects_with non-intersecting rectangle should be false" (Rectangle.intersects_with rect rect3 |> should be False)
            ]
