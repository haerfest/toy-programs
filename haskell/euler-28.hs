answer size
  | odd size = 1 + br + bl + tl + tr
  where
    tr = diag 0               -- top-right diagonal
    tl = diag 1               -- top-left diagonal
    bl = diag 2               -- bottom-left diagonal
    br = diag 3               -- bottom-right diagonal
    diag i = sum [n^2 + i * (1-n) | n <- [3,5..size]]
