#light

namespace dark

    module Common
        open System
        let randomize list =
            let random = new Random()
            let (_,rlist) = 
                list
                |> List.map (fun i -> (random.Next(), i))
                |> List.sort (fun (i1,_) (i2,_) -> i1.CompareTo(i2))
                |> List.unzip
            rlist       
                    
        let rec loop test_f apply_f x =
            match (test_f x) with
            | true -> loop test_f apply_f (apply_f x)
            | false -> x
            
        let (|Between|_|) min max num =
            if (num >= min && num <= max) then
                Some(num)
            else
                None

        let list_intersect a b =
            let setA = Set.of_list a
            let setB = Set.of_list b
            Set.to_list (Set.intersect setA setB)

        let list_diff a b =
            let setA = Set.of_list a
            let setB = Set.of_list b
            Set.to_list (Set.diff setA setB)