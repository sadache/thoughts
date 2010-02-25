#r "Dependencies/ConsoleApplication14.dll"
#r "Dependencies/DAOPersistant.dll"
#load "../Agents/AgentSystem.fs"
#load "UsefulStuff.fs"
#load "Exp.fs"
#load "Module1.fs"

// Samples
open System
open ConsoleApplication14
open Exp
open Module1
open DAOPersistant.DAORedis
open System.IO
open System.Runtime.Serialization.Formatters.Binary




let deserialize (bytes:Byte[])=using (new MemoryStream(bytes))
                                    (fun memoryStream ->
                                        let formatter = new BinaryFormatter()
                                        formatter.Deserialize(memoryStream))
                                    
let serialize obj  =using (new MemoryStream())
                            (fun memoryStream -> let formatter = new BinaryFormatter()
                                                 formatter.Serialize(memoryStream, obj)
                                                 memoryStream.ToArray())


let client= new DAORedis(new DAORedis.VersionSet(0,0,0,0),"",111):> DAOPersistant.IDAOPersistant;
let loadAll=[for entry in client.Get("C:%") -> calcStore.Add(entry.Key,(deserialize entry.Value):?> Exp)]
