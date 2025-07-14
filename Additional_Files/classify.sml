
structure NaiveBayes :> NAIVE_BAYES_CLASSIFIER =
struct

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)

    (* Purpose: The function gather takes in a mapreduceable type of (category * document)'s, which is called train (of type labeled_document MR.mapreducable).
       The function gather returns a thing of type statistics for train, which consists of:
       1. A dictionary where for each category, there's a tuple that has: (the category, the number of documents with that category) 
          (of type (category,int) Dict.dict)
       2. A dictionary where for each category, there's a tuple that has: (the category, the total number of words across all documents with that category (counting 
          duplicates)) (of type (category,int) Dict.dict)
       3. A dictionary where for each category and word, there's a tuple that has: ((the category, the word), the number of times that word occurs in 
          documents with that category) (of type ((category,string), int) Dict.dict)
       4. A sequence of all categories (with no duplicates) (of type category Seq.seq)
       5. The total number of classified documents (of type int)
       6. The total number of unique words used in all documents (so don’t count duplicates) (of type int)
     *)
    fun gather (train : labeled_document MR.mapreducable) : statistics =
      let
        val extrcomb_1 = ExtractCombine.extractcombine(String.compare, (fn ((cat, doc)) => Seq.cons((cat, 1), Seq.empty())), (fn (x,y) => x+y), train)
        
        val extrcomb_2 = ExtractCombine.extractcombine(String.compare, (fn ((cat, doc)) => Seq.cons((cat, Seq.length(doc)), Seq.empty())), (fn (x,y) => x+y), train)
        
        fun extrcomb_3_help_compare((cat1, word1), (cat2, word2)) =
          case String.compare(cat1, cat2) of
            EQUAL => String.compare(word1, word2)
            | LESS => LESS
            | GREATER => GREATER
        fun extrcomb_3_help_extract((cat, doc)) =
          Seq.map((fn (word) => ((cat, word), 1)), doc)
        val extrcomb_3 = ExtractCombine.extractcombine(extrcomb_3_help_compare, extrcomb_3_help_extract, (fn (x,y) => x+y), train)
        
        val part_4_1st = Dict.toSeq(extrcomb_2)
        val part_4_2nd = Seq.map((fn (cat, integer) => cat), part_4_1st)
        
        val part_5 = Dict.size(extrcomb_1)

        val extrcomb_6 = Dict.size(ExtractCombine.extractcombine(String.compare, (fn((cat,doc)) => Seq.map((fn (word) => (word, 1)), doc)), (fn (x,y) => x+y), train))
      in
        (extrcomb_1, extrcomb_2, extrcomb_3, part_4_2nd, part_5, extrcomb_6)
      end

        
    (* Purpose: The function possible_classifications takes in the statistics of a set of training data (num_docs_by_cat, num_words_by_cat, freqs, 
    all_categories, total_num_docs, total_num_words) (of type statistics) and a document test_doc (of type document), and returns the result of mapping
    test_doc (aka w1, ..., wn) to the sequence of all pairs (C, ln P(C | w1, ..., wn)) for each category C (of type (category * real) Seq.seq).
    *)
    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics,
         test_doc : document) : (category * real) Seq.seq = 
          let
            fun comparing((cat1, word1), (cat2, word2)) =
              case String.compare(cat1, cat2) of
                EQUAL => String.compare(word1, word2)
                | LESS => LESS
                | GREATER => GREATER

            fun result_one_cat(cat) = (let
                                          val only_cat_ln_p_c = Math.ln(Real.fromInt(Dict.lookup'(String.compare, num_docs_by_cat, cat))/Real.fromInt(total_num_docs))
                                          val num_words_this_cat = Dict.lookup'(String.compare, num_words_by_cat, cat)
                                          
                                          fun compute_value(word) =
                                            case Dict.lookup(comparing, freqs, (cat, word)) of
                                              NONE => Math.ln(1.0/Real.fromInt(total_num_words))
                                              | SOME (x) => Math.ln(Real.fromInt(x)/Real.fromInt(num_words_this_cat))
                                          
                                          val sum_cats_words = Seq.mapreduce(compute_value, 0.0, (fn (x,y) => x+y), test_doc)
                                        in
                                          only_cat_ln_p_c + sum_cats_words
                                        end
                                        )
          in
            Seq.map((fn (cat1) => (cat1, result_one_cat(cat1))), all_categories)
          end
    

    (* Purpose: The function classify takes the statistics of a training dataset of labeled documents (called stats) (of type statistics) and a document 
    test_doc (of type document) (aka w1, ..., wn) and returns a tuple of (best/maximum category, ln P(C | w1, ..., wn)) (of type category * real)
    *)
    fun classify (stats : statistics, test_doc : document) : (category * real) =
      let
        val all_cats = possible_classifications(stats, test_doc)

        fun compute_best_cat((cat1, val1), (cat2, val2)) =
          case Real.compare(val1, val2) of
            EQUAL => (cat1, val1)
            | GREATER => (cat1, val1)
            | LESS => (cat2, val2)
      in
        Seq.reduce(compute_best_cat, ("", Real.negInf), all_cats)
      end


    (* Purpose: The function train_classifier takes in a mapreduceable type of (category * document)'s, which is called train (of type labeled_document 
    MR.mapreducable), and is a training dataset of labeled documents. It then returns a classifying function that takes in a type of document and returns 
    a tuple of type (category * real).
    *)
    fun train_classifier (train : labeled_document MR.mapreducable) : document -> (category * real) =
      let
        val train_stats = gather(train)
        fun classify_fun(doc : document) : (category * real) =
          classify(train_stats, doc)
      in
        classify_fun
      end


(* Percentage my classifier gets correct:
  Small_train, Small_test: 5/8 correct (62.5% accuracy)
  Medium_train, Medium_test: 680/808 correct (84.2% accuracy)
  Big_train, Big_test: 70122/78899 correct (88.9% accuracy)
*)

        
end
