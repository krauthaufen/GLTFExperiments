namespace Aardvark.GLTF

open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive

[<Struct; CustomEquality; CustomComparison>]
type MaterialId private (value : System.Guid) =
    member private x.Value = value
    override x.GetHashCode() = Unchecked.hash value
    override x.Equals(o) =
        match o with
        | :? MaterialId as other -> value = other.Value
        | _ -> false
        
        
    static member New() = MaterialId(System.Guid.NewGuid())
        
    interface System.IComparable with
        member x.CompareTo(o) =
            value.CompareTo((o :?> MaterialId).Value)
  
        
[<Struct; CustomEquality; CustomComparison>]
type ImageId private (value : System.Guid) =
    member private x.Value = value
    override x.GetHashCode() = Unchecked.hash value
    override x.Equals(o) =
        match o with
        | :? ImageId as other -> value = other.Value
        | _ -> false
        
        
    static member New() = ImageId(System.Guid.NewGuid())
        
    interface System.IComparable with
        member x.CompareTo(o) =
            value.CompareTo((o :?> ImageId).Value)
        
[<Struct; CustomEquality; CustomComparison>]
type MeshId private (value : System.Guid) =
    member private x.Value = value
    override x.GetHashCode() = Unchecked.hash value
    override x.Equals(o) =
        match o with
        | :? MeshId as other -> value = other.Value
        | _ -> false
        
        
    static member New() = MeshId(System.Guid.NewGuid())
        
    interface System.IComparable with
        member x.CompareTo(o) =
            value.CompareTo((o :?> MeshId).Value)
        
type TexCoordSemantic =
    | BaseColor
    | Roughness
    | Metallicness
    | Normal
    | Emissive

type Material =
    {
        Name            : string
        
        DoubleSided         : bool
        Opaque              : bool  
            
        AlbedoTexutre       : option<ImageId>
        AlbedoColor         : C4f
            
        Roughness           : float
        RoughnessTexture    : option<ImageId>
        
        Metallicness        : float
        MetallicnessTexture : option<ImageId>
        
        EmissiveColor       : C4f
        EmissiveTexture     : option<ImageId>
        
        NormalTexture       : option<ImageId>
        NormalTextureScale  : float
    }

type Mesh =
    {
        Material        : option<MaterialId>
        Mode            : IndexedGeometryMode
        Index           : option<int[]>
        Positions       : V3f[]
        Normals         : option<V3f[]>
        TexCoords       : list<V2f[] * HashSet<TexCoordSemantic>>
        Colors          : option<C4b[]>
    }
    
type Node =
    {
        Trafo           : Trafo3d
        Meshes          : list<MeshId> 
        Children        : list<Node>
    }
    

type Scene =
    {
        Materials   : HashMap<MaterialId, Material>
        Meshes      : HashMap<MeshId, Mesh>
        Images      : HashMap<ImageId, PixImage>
        RootNode    : Node
    }



