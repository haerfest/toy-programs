-module(brainfuck).
-export([demo/0, run/1, run/2]).

-record(state, {program, cycle, pc, mem, dp, do_trace, add_newline}).

-spec demo() -> ok.
demo() ->
    run("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.").

-spec run(string()) -> ok.
run(Program) ->
    run(Program, false).

-spec run(string(), boolean()) -> ok.
run(Program, DoTrace) ->
    State = #state{program     = Program,
                   mem         = lists:duplicate(30000, 0),
                   dp          = 1,
                   pc          = 1,
                   cycle       = 0,
                   do_trace    = DoTrace,
                   add_newline = false},
    interpret(State).

interpret(State = #state{program = Program, pc = PC, cycle = Cycle, add_newline = AddNewline}) ->
    case PC =< length(Program) of
        true ->
            do_trace(State),
            Opcode   = lists:nth(PC, Program),
            NewState = case Opcode of
                           $+ -> inc_value_at_dp(State);
                           $- -> dec_value_at_dp(State);
                           $> -> inc_dp(State);                 
                           $< -> dec_dp(State);
                           $. -> print_value_at_dp(State);
                           $, -> read_value_into_dp(State);
                           $[ -> begin_while(State);
                           $] -> end_while(State);
                           _  -> nop(State)
                       end,
            interpret(NewState#state{cycle = Cycle + 1});
        false ->
            case AddNewline of
                true  -> io:format("~n");
                false -> ok
            end
    end.

do_trace(#state{do_trace = DoTrace, cycle = Cycle, mem = Mem, dp = DP, pc = PC, program = Program}) ->
    case DoTrace of
        true ->
            io:format("trace: ~s~n", [Program]),
            io:format("     : ~s~n", [lists:duplicate(PC - 1, $_) ++ [$^]]),
            Value  = lists:nth(DP, Mem),
            Opcode = lists:nth(PC, Program),
            PrOpc  = if
                         Opcode < $\  -> "NOP";
                         Opcode > $~  -> "NOP";
                         true         -> Opcode
                     end,
            io:format("     : Cycle=~6..0b PC=~3..0b [PC]=~b=$~c DP=~6..0b [DP]=~b~n~n", [Cycle, PC, Opcode, PrOpc, DP, Value]);
        false ->
            void
    end.

inc_value_at_dp(State) ->
    modify_value_at_dp(fun inc/1, State).

dec_value_at_dp(State) ->
    modify_value_at_dp(fun dec/1, State).

modify_value_at_dp(F, State = #state{mem = Mem, dp = DP, pc = PC}) ->
    {Left, [Value|Right]} = lists:split(DP - 1, Mem),
    NewMem = Left ++ [F(Value)] ++ Right,
    State#state{mem = NewMem, pc = PC + 1}.

inc_dp(State = #state{dp = DP, pc = PC}) ->
    State#state{dp = DP + 1, pc = PC + 1}.

dec_dp(State = #state{dp = DP, pc = PC}) ->
    State#state{dp = DP - 1, pc = PC + 1}.

print_value_at_dp(State = #state{mem = Mem, dp = DP, pc = PC}) ->
    Char = lists:nth(DP, Mem),
    io:format("~c", [Char]),
    AddNewline = Char /= $\n,
    State#state{pc = PC + 1, add_newline = AddNewline}.

read_value_into_dp(State) ->
    [Char|_Ignored] = io:get_line("? "),
    modify_value_at_dp(fun(_) -> Char end, State).

begin_while(State = #state{program = Program, pc = PC, mem = Mem, dp = DP}) ->
    case lists:nth(DP, Mem) of
        0 ->
            NewPC = find($], Program, PC + 1, fun inc/1, {$[, 0}),
            State#state{pc = NewPC};
        _ ->
            State#state{pc = PC + 1}
    end.

end_while(State = #state{program = Program, pc = PC, mem = Mem, dp = DP}) ->
    case lists:nth(DP, Mem) of
        0 ->
            State#state{pc = PC + 1};
        _ ->
            NewPC = find($[, Program, PC - 1, fun dec/1, {$], 0}),
            State#state{pc = NewPC}
    end.

find(Item, List, Pos, F, {Skip, Depth}) ->
    case lists:nth(Pos, List) of
        Item ->
            case Depth of
                0 ->
                    Pos;
                _ ->
                    find(Item, List, F(Pos), F, {Skip, Depth - 1})
            end;
        Skip ->
            find(Item, List, F(Pos), F, {Skip, Depth + 1});
        _ ->
            find(Item, List, F(Pos), F, {Skip, Depth})
    end.

inc(X) -> X + 1.
dec(X) -> X - 1.

nop(State = #state{pc = PC}) ->
    State#state{pc = PC + 1}.
