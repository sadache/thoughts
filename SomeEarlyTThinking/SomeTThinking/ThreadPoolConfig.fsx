let  minT:int ref =ref 0
let  minIOT:int ref =ref 0
let _= System.Threading.ThreadPool.GetMinThreads(minT,minIOT)
let _= System.Threading.ThreadPool.SetMinThreads(100,minIOT.contents)

