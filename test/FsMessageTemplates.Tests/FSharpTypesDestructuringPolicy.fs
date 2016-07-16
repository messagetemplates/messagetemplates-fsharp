module Destructurama

open System.Reflection
open FSharp.Reflection

// From Destructurama.FSharp
type public FSharpTypesDestructuringPolicy() =
    interface MessageTemplates.Core.IDestructuringPolicy with
        member __.TryDestructure(value,
                                 propertyValueFactory : MessageTemplates.Core.IMessageTemplatePropertyValueFactory,
                                 result: byref<MessageTemplates.Structure.TemplatePropertyValue>) =
            let cpv obj = propertyValueFactory.CreatePropertyValue(obj, true)
            let lep (n:System.Reflection.PropertyInfo) (v:obj) =
                MessageTemplates.Structure.TemplateProperty(n.Name, cpv v)
            match value.GetType() with
            | t when FSharp.Reflection.FSharpType.IsTuple t ->
                let tupleValues = value |> FSharp.Reflection.FSharpValue.GetTupleFields
                                        |> Seq.map cpv
                result <- MessageTemplates.Structure.SequenceValue(tupleValues)
                true
            | t when t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<List<_>> ->
                let objEnumerable = value :?> System.Collections.IEnumerable |> Seq.cast<obj>
                result <- MessageTemplates.Structure.SequenceValue(objEnumerable |> Seq.map cpv)
                true
            | t when FSharp.Reflection.FSharpType.IsUnion t ->
                let case, fields = FSharp.Reflection.FSharpValue.GetUnionFields(value, t)
                let properties = (case.GetFields(), fields) ||> Seq.map2 lep
                result <- MessageTemplates.Structure.StructureValue(properties, case.Name)
                true
            | _ -> false


