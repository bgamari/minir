import qualified MinIR.DiskStore as DS

main = do
    ds <- DS.new "hello"
    o1 <- DS.appendObj ds "hello World"
    o3 <- DS.appendObj ds (o1,o1)
    DS.setRoot ds $ Just o3
    print =<< DS.getRoot ds
    DS.close ds

    Just ds' <- DS.open "hello" :: IO (Maybe (DS.DiskStore (DS.Obj String, DS.Obj String)))
    Just (a,b) <- DS.getRoot ds'
    print =<< DS.getObj ds' a
