!go.

+!go : task(Stone1, Stone2, Stone3)
   <- !get_stone(Stone1);
      !get_stone(Stone2);
      !get_stone(Stone3);
      tlg(go).

+!get_stone(Stone) : item(_, Stone, _)
   <- pick.

+!get_stone(Stone) : door(Direction, Stone)
   <- go(Direction);
      pick.

+!get_stone(Stone) : door(Direction, _)
   <- go(Direction);
      !get_stone(Stone).
