open Tile

let tilesState: Recoil.readWrite<list<GameTile.tile>> = Recoil.atom({
  key: "tilesState",
  default: {
    let fst = GameTile.createNewTile(list{})
    let snd = GameTile.createNewTile(list{ fst })
    list{ fst, snd }
  }
})
