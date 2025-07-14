
structure ExtractCombine :> EXTRACT_COMBINE =
struct

    (* Purpose: The function extractcombine takes in:
       a comparison function k_cmp (of type 'k * 'k -> order) that compares keys,
       an extractor function extract (of type 'a -> ('k * 'v) Seq.seq) that extracts a key-value sequence (of type ('k * 'v) Seq.seq) from a single mapreduceable thing (of type 'a) (such as a document),
       a combiner function v_combine (of type ('v * 'v -> 'v)) that combines values (of type 'v) when the same key occurs multiple times in the extracted information,
       and a mapreduceable type of documents docs (of type 'a MR.mapreducable).
       The function extractcombine returns a key-value dictionary (of type ('k, 'v) Dict.dict) consisting of all of the keys extracted from all of the documents
       in docs, where the dictionary maps a key to the combination of all of its extracted values.
    *)
    fun extractcombine(k_cmp : ('k * 'k -> order), extract : ('a -> ('k * 'v) Seq.seq), v_combine : ('v * 'v -> 'v), docs : 'a MR.mapreducable) : ('k,'v) Dict.dict = 
        MR.mapreduce((fn (doc1) => Seq.mapreduce((fn ((k, v)) => Dict.insert(k_cmp, Dict.empty, (k, v))), Dict.empty, (fn (dict1, dict2) => Dict.merge(k_cmp, v_combine, dict1, dict2)), extract(doc1))), 
                      Dict.empty, 
                      (fn (t1, t2) => Dict.merge(k_cmp, v_combine, t1, t2)), 
                      docs)

end

