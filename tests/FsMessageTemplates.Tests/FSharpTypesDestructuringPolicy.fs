module Destructurama

// From Destructurama.FSharp
type public FSharpTypesDestructuringPolicy() =
    interface MessageTemplates.Core.IDestructuringPolicy with
        member __.TryDestructure(value,
                                 propertyValueFactory : MessageTemplates.Core.ITemplatePropertyValueFactory,
                                 result: byref<MessageTemplates.Structure.TemplatePropertyValue>) =
            let cpv obj = propertyValueFactory.CreatePropertyValue(obj, true)
            let lep (n:System.Reflection.PropertyInfo) (v:obj) =
                MessageTemplates.Structure.TemplateProperty(n.Name, cpv v)
            match value.GetType() with
            | t when Microsoft.FSharp.Reflection.FSharpType.IsTuple t ->
                let tupleValues = value |> Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields
                                        |> Seq.map cpv
                result <- MessageTemplates.Structure.SequenceValue(tupleValues)
                true
            | t when t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<List<_>> ->
                let objEnumerable = value :?> System.Collections.IEnumerable |> Seq.cast<obj>
                result <- MessageTemplates.Structure.SequenceValue(objEnumerable |> Seq.map cpv)
                true
            | t when Microsoft.FSharp.Reflection.FSharpType.IsUnion t ->
                let case, fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, t)
                let properties = (case.GetFields(), fields) ||> Seq.map2 lep
                result <- MessageTemplates.Structure.StructureValue(properties, case.Name)
                true
            | _ -> false


