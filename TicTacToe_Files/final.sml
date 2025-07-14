(* sequence helper function *)
structure SeqUtils =
struct

    (* determines whether there exists an element of a sequence satisfying
       the predicate (aka function) p (see lab 8)
     *)
    fun seqExists(p : 'a -> bool, s : 'a Seq.seq) : bool =
        Seq.mapreduce(p, false, fn (x,y) => x orelse y, s)

end

structure Player =
struct

    datatype player = X | O

    fun player_to_string (p : player) : string =
        case p of
            X => "X"
          | O => "O"

    fun next (p : player) : player =
        case p of
            X => O
          | O => X

    datatype status = Playing | Won of player | Draw 
        
end

signature GAME =
sig

    type board

    val show_board : board -> string

    type state = board * Player.player 
        
    val start : state
        
    val hash : state -> string

    val check_status : state -> Player.status

    type move

    val parse_move : state * string -> move option
    val show_move : move -> string

    val possible_moves : state -> move Seq.seq

    val make_move : state * move -> state
        
end


(* Implementation of the game Tic-Tac-Toe: *)
structure TicTacToe =
struct

    open Player

    (* In ((s * i), v), (s * i) represents the square ID (where s is the letter and i is the number)(for example, a * 3, b * 1, c * 2), and v represents the mark in the square (X, O, or _) *)
    type board = ((string * int) * string) Seq.seq

    (* A board is n_size by n_size *)
    val n_size = 3

    fun show_board(board1) =
        let
            (* A sequence of all 26 letters in the alphabet *)
            val letters = Seq.fromlist(["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"])

            fun hash_letter(num) =
                case num>26 of
                    true => hash_letter(num-26) ^ "a"
                    | false => Seq.nth(num-1, letters)

            fun print_letter_row(num) =
                case num of
                    0 => "  "
                    | _ => print_letter_row(num-1) ^ "  " ^ hash_letter(num)

            fun print_board_rows(num) =
                let
                    fun print_a_row(num_to_print, row) =
                        case num_to_print of
                            0 => ""
                            | _ => (let
                                        val ((square_lett, square_num), square_mark) = Seq.nth((((row-1)*n_size) + num_to_print - 1), board1)
                                    in
                                        print_a_row(num_to_print-1, row) ^ "[" ^ square_mark ^ "]"
                                    end
                                    )
                in
                    case num of
                        0 => ""
                        | _ => (case num<10 of
                                    true => print_board_rows(num-1) ^ "\n" ^ Int.toString(num) ^ "  " ^ print_a_row(n_size, num)
                                    | false => print_board_rows(num-1) ^ "\n" ^ Int.toString(num) ^ " " ^ print_a_row(n_size, num)
                               )
                end
                
        in
            print_letter_row(n_size) ^ print_board_rows(n_size)
        end
    
    type state = board * player
    
    val start = 
        let
            val letters = Seq.fromlist(["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"])

            fun hash_letter(num) =
                case num>26 of
                    true => hash_letter(num-26) ^ "a"
                    | false => Seq.nth(num-1, letters)

            fun create_empty_board(row_num) =
                case row_num of
                    0 => Seq.fromlist([])
                    | _ => (let
                                (* The sequence for one row (for example: ((a * 1), _), ((b * 1), _), ((c * 1), _) *)
                                fun one_row_seq(n_size1, row) = 
                                    Seq.tabulate((fn (index) => ((hash_letter(index+1), row), "_")), n_size)
                            in
                                Seq.append(create_empty_board(row_num-1), one_row_seq(n_size, row_num))
                            end)
        in
            (create_empty_board(n_size), O)
        end

    fun hash(board1, player1) = Seq.toString((fn(((s, i), mark)) => s ^ Int.toString(i) ^ mark), board1) ^ player_to_string player1

    fun check_status(board1, player1) =
        let
            (* Note: Let the Tic Tac Toe board be a n_size by n_size board. I'm defining winning as: a player gets either n_size marks in a row horizontally,
               n_size marks in a row vertically, and/or n_size marks in a row diagonally. This means that the player fills either an entire row, column, and/or 
               diagonal
             *)
            
            (* Where player1 and player2 are the two players playing the game *)
            val player2 = next(player1)

            val letters_dict = 
                let
                    val lett_seq = Seq.fromlist([("a",1),("b",2),("c",3),("d",4),("e",5),("f",6),("g",7),("h",8),("i",9),("j",10),("k",11),("l",12),("m",13),("n",14),("o",15),("p",16),("q",17),("r",18),("s",19),("t",20),("u",21),("v",22),("w",23),("x",24),("y",25),("z",26)])
                in
                    Seq.mapreduce((fn ((s, i)) => Dict.insert(String.compare, Dict.empty, (s, i))), Dict.empty, (fn (d1, d2) => Dict.merge(String.compare, (fn (x,y) => x+y), d1, d2)), lett_seq)
                end

            fun hash_number(letter) = Dict.lookup'(String.compare, letters_dict, letter)

            val letters = Seq.fromlist(["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"])

            fun hash_letter(num) =
                case num>26 of
                    true => hash_letter(num-26) ^ "a"
                    | false => Seq.nth(num-1, letters)

            fun only_nth_row(n) = Seq.filter((fn (((id_s, id_i), mark)) => id_i=n), board1)

            fun only_nth_column(n) = Seq.filter((fn (((id_s, id_i), mark)) => id_s=n), board1)

            val only_top_to_bott_diagonal = Seq.filter((fn (((id_s, id_i), mark)) => hash_number(id_s)=id_i), board1)

            val only_bott_to_top_diagonal = Seq.filter((fn (((id_s, id_i), mark)) => (n_size - hash_number(id_s) + 1)=id_i), board1)

            (* Note: I remember that in a lab you mentioned <> meant "not equal to" in SML *)

            (* Checks if a player (player0) filled an entire row, column, or diagonal (which is section1). Section1 is either only_nth_row(n), only_nth_column(n),
               only_top_to_bott_diagonal, or only_bott_to_top_diagonal *)
            fun player_filled_all(player0, section1) = 
                case SeqUtils.seqExists((fn (((id_s, id_i), mark)) => mark<>player_to_string(player0)), section1) of
                    true => false
                    | false => true

            (* Returns true if for ANY of the rows (from row 1 until row n), the player_filled_all function returns true. Otherwise, it returns false *)
            fun any_row_filled(player0, n) =
                case n of
                    0 => false
                    | _ => (case player_filled_all(player0, only_nth_row(n)) of
                                true => true
                                | false => any_row_filled(player0, n-1)
                            )

            (* Returns true if for ANY of the columns (from column a until column hash_number(n)), the player_filled_all function returns true. Otherwise,
            it returns false *)
            fun any_column_filled(player0, n) =
                case n of
                    "a" => player_filled_all(player0, only_nth_column(n))
                    | _ => (case player_filled_all(player0, only_nth_column(n)) of
                                true => true
                                | false => any_column_filled(player0, hash_letter(hash_number(n)-1))
                            )
                            
            (* Returns true if the entirety of board1 has either an X or an O in it (and no squares are empty). Otherwise, it returns false *)
            val whole_board_full = 
                let
                    val only_full_squares = Seq.filter((fn (((id_s, id_i), mark)) => case mark="X" of true => true | false => (case mark="O" of true => true | false => false)), board1)
                in
                    case Seq.length(only_full_squares)=Seq.length(board1) of
                        true => true
                        | false => false
                end

            (* Returns true if player0 won. Otherwise, it returns false. *)
            fun did_player_win(player0) = 
                any_row_filled(player0, n_size) orelse any_column_filled(player0, hash_letter(n_size)) orelse player_filled_all(player0, only_top_to_bott_diagonal) orelse player_filled_all(player0, only_bott_to_top_diagonal)
        in
            case did_player_win(player1) of
                true => Won (player1)
                | false => (case did_player_win(player2) of
                                true => Won (player2)
                                | false => (case whole_board_full of
                                                true => Draw
                                                | false => Playing
                                            )
                            )
        end

    (* Where a move is a string with the column letter and the row number (examples: a1, b3, c2) *)
    type move = string

    fun possible_moves(board1, player1) =
        Seq.map((fn ((s1, i1), mark1) => s1 ^ Int.toString(i1)), Seq.filter((fn ((s2, i2), mark2) => mark2="_"), board1))

    fun parse_move((board1, player1), move1) =
        let
            val poss_moves_with_newline = Seq.map((fn (move1) => move1 ^ "\n"), possible_moves(board1, player1))
        in
            case SeqUtils.seqExists((fn (move2) => case String.compare(move2, move1) of EQUAL => true | _ => false), poss_moves_with_newline) of
                true => SOME move1
                | false => NONE
        end

    fun show_move(move1) = move1

    fun make_move((board1, player1), move1) =
        let
            val add_newline_x = case player1 of   
                                    X => "\n"
                                    | O => ""
            (* This changes the mark in the move1 square to player1's mark*)
            val new_board = Seq.map((fn (((s1, i1), mark1)) => case (s1 ^ Int.toString(i1) ^ add_newline_x)=move1 of true => ((s1, i1), player_to_string(player1)) | false => ((s1, i1), mark1)), board1)
        in
            (new_board, next(player1))
        end

    (* Note:

    Board visual:
       a  b  c
    1 [_][_][_]
    2 [_][_][_]
    3 [_][_][_]

    Board example:
    Sequence: (a1, _) (b1, _) (c1, _) (a2, _) (b2, _) (c2, _) (a3, _) (b3, _) (c3, _)

    *)

end

structure Game :> GAME = TicTacToe

signature TRAIN =
sig

    type memory = (string,real) Dict.dict

    val best_next_state : memory * Game.state -> Game.state * real
        
    val train : int * memory * Game.state -> memory

end 

structure Train :> TRAIN =
struct

    open Player

    local
        val r = Random.rand (05092025, 3340975)
    in
        (* assuming low <= hi, generate a random integer 
           in the range [low...hi] including both low and hi *)
        fun randRange (low : int,hi : int) : int =
            Random.randRange(low,hi) r 
    end

    type memory = (string,real) Dict.dict

    val update_rate = 0.3
        
    fun best_next_state(mem : memory, (board, who) : Game.state) : Game.state * real =
        case Game.check_status(board, who) of
                Playing => (let                                
                                (* Sequence of (move1, hashed1, score1), where move1 is a possible move, hashed1 is the result of plugging the state after move1 is made into Game.hash, 
                                   and score1 is hashed1's associated score *)
                                val scores_of_hashed_possibils = 
                                    Seq.map((fn (move1) => 
                                                let 
                                                    val made_move = Game.make_move((board, who), move1)
                                                    val hashed_move1 = Game.hash(made_move) 
                                                    val new_score =
                                                        case Game.check_status(made_move) of
                                                            Won (X) => 1.0
                                                            | Won (O) => ~1.0
                                                            | Draw => 0.0
                                                            | Playing => (case Dict.lookup(String.compare, mem, hashed_move1) of 
                                                                            SOME x => x 
                                                                            | NONE => 0.0)
                                                in 
                                                    (move1, hashed_move1, new_score)
                                                end), 
                                            Game.possible_moves(board, who))

                                (* This function takes in a function that takes in two scores and returns one of the scores (in this case, it's either the larger or smaller score), 
                                   and returns a result of type (state, real). This function is used for both Player X and Player O *)
                                fun player_result(bigger_smaller_func) =
                                    let
                                        (* Does Seq.map to scores_of_hash_possibils to get a sequence of just the scores, and then reduces to get the target score (the highest or lowest score) *)
                                        val target_score = Seq.mapreduce((fn ((move1, hashed1, score1)) => score1), 0.0, bigger_smaller_func, scores_of_hashed_possibils)
                                        (* Then, filters the elements in scores_of_hash_possibils to get a sequence of all the moves whose score is equal to target_score. The elements of 
                                        all_target_moves are in the form (move, hashed, score) *)
                                        val all_target_moves = Seq.filter((fn ((move1, hashed1, score1)) => case Real.abs(score1 - target_score) < 0.0001 of true => true | false => false), scores_of_hashed_possibils)
                                        (* Accounts for the possibility that all_target_moves is empty *)
                                        val seq_we_use = 
                                            case Seq.length(all_target_moves) of
                                                0 => scores_of_hashed_possibils
                                                | _ => all_target_moves
                                        (* Picks a random element in all_highest_moves *)
                                        val (random_move, random_hash, random_score) = Seq.nth(randRange(0, (Seq.length(seq_we_use)-1)), seq_we_use)
                                    in
                                        (Game.make_move((board, who), random_move), random_score)
                                    end
                            in
                                player_result((fn (score1, score2) => case score1>score2 of true => score1 | false => score2))
                            end)
                | Won (player00) => let
                                        val one_or_neg_one = case player00 = who of
                                                                true => 1.0
                                                                | false => ~1.0
                                    in
                                        ((board, who), one_or_neg_one)
                                    end
                | Draw => ((board, who), 0.0)

    fun train(n : int, mem : memory, (board,who) : Game.state) = 
        let
            val next_who = next(who)
        in
            case n of
                0 => mem
                | _ => (case Game.check_status(board, who) of
                                Playing => (let
                                                val ((best_board, next_player), s2_score) = best_next_state(mem, (board, who))
                                                val hashed_s1 = Game.hash((board, who))
                                                val old_s1_score = 
                                                    case Dict.lookup(String.compare, mem, hashed_s1) of
                                                        SOME old_score => old_score
                                                        | NONE => 0.0
                                                val new_s1_score = old_s1_score + (update_rate * (s2_score - old_s1_score))
                                                (* Inserts (hashed_s1, new_s1_score) into mem if hashed_s1 isn't in mem, and replaces (hashed_s1, old_s1_score) in mem with 
                                                (hashed_s1, new_s1_score) if hashed_s1 is already in mem *)
                                                val new_mem = Dict.insert(String.compare, mem, (hashed_s1, new_s1_score))
                                                val ((new_best_board, new_next_player), new_s2_score) = best_next_state(new_mem, (board, who))
                                            in
                                                train(n-1, new_mem, (new_best_board, new_next_player))
                                            end
                                            )
                                | Won (player3) => (let
                                                        val one_or_neg_one = case player3 = who of
                                                                                true => 1.0
                                                                                | false => ~1.0
                                                        val hashed_s1 = Game.hash((board, who))
                                                        val new_mem = Dict.insert(String.compare, mem, (hashed_s1, 1.0))
                                                    in
                                                        train(n-1, new_mem, Game.start)
                                                    end)
                                | Draw => (let
                                                val hashed_s1 = Game.hash((board, who))
                                                val new_mem = Dict.insert(String.compare, mem, (hashed_s1, 0.0))
                                           in
                                                train(n-1, new_mem, Game.start)
                                           end)
                        )
        end
        
end 

structure Controller =
struct

    (* this means you can write the variables bound in the Player module without writing Player.variable,
       e.g. X instead of Player.X *)
    open Player 
    
    val mem = Train.train(100000, Dict.empty, Game.start)

    fun controller (s : Game.state) : unit = 
        (* Playing against an AI *)

        let
            (* The user/human player is X. The AI is O. *)

            val (s_board, s_player) = s

            val shows_board = print(Game.show_board(s_board))

            val print_space = print("\n")
        in
            case Game.check_status(s) of
                Playing => (case s_player of
                                O => (let
                                        val making_move = print("\nPlayer O is choosing a move\n\n")
                                        val ((new_board, human_player), real_val) = Train.best_next_state(mem, (s_board, s_player))
                                      in
                                        controller((new_board, human_player))
                                      end
                                     )
                                | X => (let
                                            val prompt = print("\nPlayer X enter your move. The possibilities are\n  ")
                                            val poss_minus_last = Seq.take(Seq.length(Game.possible_moves(s))-1, Game.possible_moves(s))
                                            val poss_last = Seq.drop(Seq.length(Game.possible_moves(s))-1, Game.possible_moves(s))
                                            val print_poss_minus_last = print(Seq.mapreduce((fn (move) => Game.show_move(move)), "", (fn(move1_str, move2_str) => move1_str ^ ", " ^ move2_str), poss_minus_last))
                                            val print_poss_last = print(Seq.mapreduce((fn (move) => Game.show_move(move)), "", (fn(move1_str, move2_str) => move1_str ^ " " ^ move2_str), poss_last))
                                            val choose_move = TextIO.inputLine TextIO.stdIn
                                        in
                                            case choose_move of
                                                SOME user_move => (case Game.parse_move(s, user_move) of
                                                                        NONE => (let
                                                                                    val redo = print("Please enter a valid move\n\n")
                                                                                in
                                                                                    controller(s)
                                                                                end)
                                                                        | SOME user_move0 => (let
                                                                                                val print_lines = print("\n")
                                                                                              in
                                                                                                controller(Game.make_move(s, user_move0))
                                                                                              end
                                                                                             )
                                                                        )
                                                | NONE => (let
                                                                val redo = print("Please enter a move\n\n")
                                                           in
                                                                controller(s)
                                                           end)
                                        end
                                       )
                            )
                | Won (w_player) => print("\n" ^ player_to_string(w_player) ^ " wins!\n")
                | Draw => print("It's a draw!\n")
        end

    fun go() = controller(Game.start)

end


