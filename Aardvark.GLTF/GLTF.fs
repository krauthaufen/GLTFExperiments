namespace Aardvark.GLTF

open System.IO
open glTFLoader
open glTFLoader.Schema
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Microsoft.FSharp.NativeInterop
open Aardvark.GLTF

#nowarn "9"


module GLTF =
    
    let private getArray<'a when 'a : unmanaged> (byteOffset : int) (byteStride : int) (cnt : int) (buffer : byte[]) =
        let size = sizeof<'a>
        
        if byteStride = 0 || byteStride = size then
            let arr = Array.zeroCreate<'a> cnt
            use ptr = fixed arr
            let src = System.Span(buffer, byteOffset, buffer.Length - byteOffset)
            let dst = System.Span<byte>(NativePtr.toVoidPtr ptr, arr.Length * size)
            src.Slice(0, arr.Length * size).CopyTo(dst)
            arr
        else
            let res = Array.zeroCreate<'a> cnt
            use pSrc = fixed buffer
            use pDst = fixed res
            
            let mutable pSrc = NativePtr.ofNativeInt<'a> (NativePtr.toNativeInt pSrc + nativeint byteOffset)
            let mutable pDst = pDst
            for i in 0 .. cnt - 1 do
                NativePtr.write pDst (NativePtr.read pSrc)
                pSrc <- NativePtr.ofNativeInt (NativePtr.toNativeInt pSrc + nativeint byteStride)
                pDst <- NativePtr.add pDst 1
            res
    
    let private getAttributeArray (readBuffer : int -> byte[]) (model : Gltf) (accId : int) =
        let acc = model.Accessors.[accId]
        if acc.BufferView.HasValue then
            let view = model.BufferViews.[acc.BufferView.Value]
            let bufferData = readBuffer view.Buffer
            let stride = if view.ByteStride.HasValue then view.ByteStride.Value else 0
            
            let byteOffset = view.ByteOffset + acc.ByteOffset
            
            match acc.ComponentType with
            | Accessor.ComponentTypeEnum.UNSIGNED_BYTE ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<byte> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<C3b> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<C4b> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.UNSIGNED_SHORT ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<uint16> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<C3us> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<C4us> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.UNSIGNED_INT->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<uint32> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC2 -> getArray<V2ui> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<V3ui> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<V4ui> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.FLOAT ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<float32> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC2 -> getArray<V2f> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC3 -> getArray<V3f> byteOffset stride acc.Count bufferData :> System.Array
                | Accessor.TypeEnum.VEC4 -> getArray<V4f> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.SHORT ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<int16> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | Accessor.ComponentTypeEnum.BYTE ->
                match acc.Type with
                | Accessor.TypeEnum.SCALAR -> getArray<int8> byteOffset stride acc.Count bufferData :> System.Array
                | _ -> failwith ""
            | _ ->
                failwith ""
        else
            failwith ""
    
    let private flipY (arr : System.Array) =
        match arr with
        | :? array<V2f> as arr -> arr |> Array.map (fun v -> V2f(v.X, 1.0f - v.Y)) :> System.Array
        | :? array<V2d> as arr -> arr |> Array.map (fun v -> V2d(v.X, 1.0 - v.Y)) :> System.Array
        | _ -> failwithf "bad TC type: %A" arr
    
    let private getTrafo (node : glTFLoader.Schema.Node) =

        let mutable trafo = Trafo3d.Identity
        
        if not (isNull node.Matrix) then
            let m = node.Matrix
            let fw =
                M44d(
                    float m.[0], float m.[4], float m.[8], float m.[12],
                    float m.[1], float m.[5], float m.[9], float m.[13],
                    float m.[2], float m.[6], float m.[10], float m.[14],
                    float m.[3], float m.[7], float m.[11], float m.[15]
                )
            trafo <- trafo * Trafo3d(fw, fw.Inverse)
            
        if not (isNull node.Scale) then
            let s = node.Scale
            trafo <- trafo * Trafo3d.Scale(float s.[0], float s.[1], float s.[2])
        
        if not (isNull node.Rotation) then
            let q = node.Rotation
            let q = QuaternionD(float q.[3], float q.[0], float q.[1], float q.[2])
            let q = Rot3d(q.Normalized)
            
            let q : Trafo3d = q |> Rot3d.op_Explicit
            trafo <- trafo * q
            
        if not (isNull node.Translation) then
            let t = node.Translation
            trafo <- trafo * Trafo3d.Translation(V3d(float t.[0], float t.[1], float t.[2]))
            
        trafo
            
    let private toScene (file : string) (model : Gltf) =
        
        let bufferCache = Dict<int, byte[]>()
        
        let readBuffer (i : int) =
            bufferCache.GetOrCreate(i, fun i ->
                let buffer = model.Buffers.[i]
                model.LoadBinaryBuffer(i, file)    
            )
        
        let images =
            if isNull model.Images || model.Images.Length = 0 then
                [||]
            else
                model.Images |> Array.mapi (fun i img ->
                    if img.BufferView.HasValue then
                        let id = ImageId.New()
                        let view = model.BufferViews.[img.BufferView.Value]
                        let buffer = readBuffer view.Buffer
                        
                        try
                            let pimg = 
                                use ms = new MemoryStream(buffer, view.ByteOffset, view.ByteLength)
                                PixImage.Load(ms)
                                
                            Some (id, pimg)
                        with e ->
                            Log.warn "could not load image %A (%A)" img.Name img.Uri
                            None
                    else
                        None
                )
        

        let materials =
            if isNull model.Materials then
                [||]
            else
                model.Materials |> Array.mapi (fun mi mat ->
                    let id = MaterialId.New()
                    let mutable tcMapping = HashMap.empty<int, HashSet<TexCoordSemantic>>
                    
                    let opaque = mat.AlphaMode <> Material.AlphaModeEnum.BLEND
                    let doubleSided = mat.DoubleSided
                    
                    let tryGetTextureWithIndex (sem : TexCoordSemantic) (index : int) (tcIndex : int) =
                        let tex = model.Textures.[index]
                        if tex.Source.HasValue then
                            match images.[tex.Source.Value] with
                            | Some (imageId, _) ->
                                tcMapping <-
                                    tcMapping
                                    |> HashMap.alter tcIndex (function
                                        | Some o -> Some (HashSet.add sem o)
                                        | None -> Some (HashSet.single sem)
                                    )
                                Some imageId
                            | None ->
                                None
                        else
                            None
                
                    let tryGetTexture (sem : TexCoordSemantic) (info : TextureInfo) =
                        if isNull info then
                            None
                        else
                            tryGetTextureWithIndex sem info.Index info.TexCoord
                    
                    let albedoColor =
                        if isNull mat.PbrMetallicRoughness then C4f(1.0f, 1.0f, 1.0f, 1.0f)
                        else C4f mat.PbrMetallicRoughness.BaseColorFactor
                    
                    let albedoTexture =
                        if isNull mat.PbrMetallicRoughness then None
                        else tryGetTexture TexCoordSemantic.BaseColor mat.PbrMetallicRoughness.BaseColorTexture
                        
                    let roughness =
                        if isNull mat.PbrMetallicRoughness then 1.0
                        else float mat.PbrMetallicRoughness.RoughnessFactor
                        
                    let roughnessTexture =
                        if isNull mat.PbrMetallicRoughness then None
                        else tryGetTexture TexCoordSemantic.Roughness mat.PbrMetallicRoughness.MetallicRoughnessTexture
                        
                    let metallicness =
                        if isNull mat.PbrMetallicRoughness then 0.0
                        else float mat.PbrMetallicRoughness.MetallicFactor
                        
                    let emissive =
                        if isNull mat.EmissiveFactor then C4f(0.0f, 0.0f, 0.0f, 0.0f)
                        else C3f(mat.EmissiveFactor).ToC4f()
                        
                    let emissiveTexture =
                        tryGetTexture TexCoordSemantic.Emissive mat.EmissiveTexture
                        
                    let normalTexture =
                        if isNull mat.NormalTexture then None
                        else tryGetTextureWithIndex TexCoordSemantic.Normal mat.NormalTexture.Index mat.NormalTexture.TexCoord
                        
                    let normalTextureScale =
                        if isNull mat.NormalTexture then 1.0
                        else float mat.NormalTexture.Scale
                        
                    id, {
                        Name                = mat.Name
                            
                        DoubleSided         = doubleSided
                        Opaque              = opaque
                            
                        AlbedoTexutre       = albedoTexture
                        AlbedoColor         = albedoColor
                        Roughness           = roughness
                        RoughnessTexture    = roughnessTexture
                        
                        Metallicness        = metallicness
                        MetallicnessTexture = None
                        
                        EmissiveColor       = emissive
                        EmissiveTexture     = emissiveTexture
                        
                        NormalTexture       = normalTexture
                        NormalTextureScale  = normalTextureScale
                    }, tcMapping
                )
    
        let meshes =
            if isNull model.Meshes then
                [||]
            else
                model.Meshes |> Array.map (fun m ->
                    let meshes = 
                        m.Primitives |> Array.choose (fun p ->
                            let index, indexRange =
                                if p.Indices.HasValue then
                                    let acc = model.Accessors.[p.Indices.Value]
                                    let minIndex = if isNull acc.Min then 0 else int acc.Min.[0]
                                    let maxIndex = if isNull acc.Max then System.Int32.MaxValue else int acc.Max.[0]
                                    getAttributeArray readBuffer model p.Indices.Value, Some (Range1i(minIndex, maxIndex))
                                else
                                    null, None
                                    
                            let attributes =
                                p.Attributes |> Seq.toArray |> Array.map (fun (KeyValue(name, att)) ->
                                    let arr = getAttributeArray readBuffer model att
                                    let arr =
                                        if name.StartsWith "TEXCOORD" then flipY arr
                                        else arr
                                    
                                    name, arr
                                )
                                
                            let attributeMap =
                                HashMap.ofArray attributes
                              
                              
                              
                            match HashMap.tryFind "POSITION" attributeMap with
                            | Some (:? array<V3f> as position) ->  
                                let bounds =
                                    let acc = model.Accessors.[p.Attributes.["POSITION"]]
                                    let l = V3f acc.Min
                                    let h = V3f acc.Max
                                    Box3d(V3d l, V3d h)
                                    
                                let mode =
                                    match p.Mode with
                                    | MeshPrimitive.ModeEnum.POINTS -> IndexedGeometryMode.PointList
                                    | MeshPrimitive.ModeEnum.LINES -> IndexedGeometryMode.LineList
                                    | MeshPrimitive.ModeEnum.LINE_STRIP -> IndexedGeometryMode.LineStrip
                                    | MeshPrimitive.ModeEnum.TRIANGLES -> IndexedGeometryMode.TriangleList
                                    | MeshPrimitive.ModeEnum.TRIANGLE_STRIP -> IndexedGeometryMode.TriangleStrip
                                    | m -> failwithf "bad mode: %A" m // TODO: convert to TriangleList indices??
                                    
                                let material =
                                    if p.Material.HasValue then
                                        let mid, _mat, _mapping = materials.[p.Material.Value]
                                        Some mid
                                    else
                                        None
                                let tcMapping =
                                    if p.Material.HasValue then
                                        let _mid, _mat, mapping = materials.[p.Material.Value]
                                        mapping
                                    else
                                        HashMap.empty
                                    
                                let texCoords =
                                    attributes |> Array.choose (fun (name, arr) ->
                                        if name.StartsWith "TEXCOORD_" then
                                            match System.Int32.TryParse (name.Substring 9) with
                                            | (true, id) ->
                                                match arr with
                                                | :? array<V2f> as arr ->
                                                    match HashMap.tryFind id tcMapping with
                                                    | Some sems -> Some (arr, sems)
                                                    | None -> None
                                                | arr ->
                                                    None
                                            | _ -> None
                                        else
                                            None
                                    )
                                    
                                let index =
                                    match index with
                                    | null -> null
                                    | :? array<uint8> as arr -> Array.map int arr
                                    | :? array<uint16> as arr -> Array.map int arr
                                    | :? array<uint32> as arr -> Array.map int arr
                                    | _ -> failwithf "unexpected index-type: %A" index
                                    
                                let normals =
                                    match HashMap.tryFind "NORMAL" attributeMap with
                                    | Some (:? array<V3f> as normals) -> Some normals
                                    | _ -> None
                                    
                                let tangents =
                                    match HashMap.tryFind "TANGENT" attributeMap with
                                    | Some (:? array<V4f> as tangents) -> Some tangents
                                    | _ -> None
                                    
                                let colors =
                                    match HashMap.tryFind "COLOR_0" attributeMap with
                                    | Some (:? array<C4b> as cs) -> Some cs
                                    | _ -> None
                                    
                                let mesh =
                                    {
                                        BoundingBox     = bounds
                                        Material        = material
                                        Mode            = mode
                                        Index           = if isNull index then None else Some index
                                        Positions       = position
                                        Normals         = normals
                                        Tangents        = tangents  
                                        TexCoords       = Array.toList texCoords
                                        Colors          = colors
                                    }
                                Some (MeshId.New(), mesh)
                            | m ->
                                Log.warn "mesh has incompatible positions: %A" m
                                None
                             
                        )
                        
                    meshes, m.Name
                )
            
        let roots =
            
            let rec traverse (nid : int) : Node * Box3d =
                let node = model.Nodes.[nid]
                let trafo = getTrafo node
                
                let cs =
                    if isNull node.Children then []
                    else node.Children |> Array.toList |> List.map traverse
                    
                let mutable bounds = cs |> Seq.map snd |> Box3d
                    
                let geometry =
                    if node.Mesh.HasValue then
                        let arr, name = meshes.[node.Mesh.Value]
                        for (_, m) in arr do
                            bounds.ExtendBy m.BoundingBox
                        let meshes = arr |> Array.map fst |> Array.toList
                        Some {
                            Name = if System.String.IsNullOrEmpty name then None else Some name
                            Meshes = meshes
                        }
                    else
                        None
                    
                let bounds = bounds.Transformed trafo
                    
                {
                    Trafo = trafo
                    Children = List.map fst cs
                    Geometry = geometry
                }, bounds
                
            if model.Scene.HasValue then
                let scene = model.Scenes.[model.Scene.Value]
                scene.Nodes |> Array.map (fun r ->
                    traverse r
                )
            else
                [||]
            
        let bounds, root = 
            match roots with
            | [||] -> Box3d.Invalid, { Trafo = Trafo3d.Identity; Geometry = None; Children = [] }
            | [|(r, b)|] -> b, r
            | rs ->
                let bounds = rs |> Array.map snd |> Box3d
                bounds, { Trafo = Trafo3d.Identity; Geometry = None; Children = List.map fst (Array.toList rs) }
            
            
        {
            BoundingBox = bounds
            Meshes      = meshes |> Array.map fst |> Array.concat |> HashMap.ofArray
            Materials   = materials |> Array.map (fun (a,b,_) -> a,b) |> HashMap.ofArray
            Images      = images |> Array.choose id |> HashMap.ofArray
            RootNode    = root
        }
    
    let loadScene (file : string) =
        let model = Interface.LoadModel file
        toScene file model
 
    

