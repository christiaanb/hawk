module Main where
import Palette

cpu () = wire12
  where
    wire11 = alu wire10
    wire12@wire13 = delay wire
    wire18 = delay wire9
    wire16 = delay wire15
    wire14 = id wire13
    wire10 = regFile wire18 wire14
    wire = sumData wire11
    (wire9, wire15) = sumInstrs wire16

main =
  do{ clearProbes_MS
    ; putStr (show $ take 100 $ view $ cpu ())
    }
