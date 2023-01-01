module Helpers

    let getCellsInDirection (starting: int * int) (xoffset, yoffset) (arr: 'T[,]) =
        let (w, h) = arr.GetLength(0), arr.GetLength(1)

        seq {
            let mutable x, y = starting
            x <- x + xoffset
            y <- y + yoffset

            while x >= 0 && x <= w-1 && y >= 0 && y <= h-1 do
                yield arr[x, y]
                x <- x + xoffset
                y <- y + yoffset
        }

    let getCellsInDirection3D (starting: int * int * int) (xoffset, yoffset, zoffset) (arr: 'T[,,]) =
        let w, h, d = arr.GetLength(0), arr.GetLength(1), arr.GetLength(2)

        seq {
            let mutable x, y, z = starting
            x <- x + xoffset
            y <- y + yoffset
            z <- z + zoffset

            while x >= 0 && x <= w-1 && y >= 0 && y <= h-1 && z >=0 && z <= d-1 do
                yield arr[x, y, z]
                x <- x + xoffset
                y <- y + yoffset
                z <- z + zoffset
        }