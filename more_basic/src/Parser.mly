%token <int> INTEGER
%token PLAY DISCARD COLORCLUE RANKCLUE
%token COMMA NEWLINE COLON (*SEMICOL*)
%token EOF

%{
%}
%type <Hanabi_types.record> record
%start record
%%

(* parameterised rules *)
my_nonempty_list(X):
| h = X t = list(X) { (h, t) }


my_separated_nonempty_list(SEP, X):
| h = X { (h, []) }
| h = X SEP t = separated_list(SEP, X) { (h, t) } (* TODO might be improvable *)

card:
  | color = INTEGER COLON rank = INTEGER {color, rank}
hand:
  | l = my_separated_nonempty_list(COMMA, card)  NEWLINE { l }

step:
  | PLAY i = INTEGER c = card     {Hanabi_types.Play(i, c)}
  | DISCARD i = INTEGER c = card  {Hanabi_types.Discard(i, c)}
  | COLORCLUE i = INTEGER         {Hanabi_types.ColorClue i}
  | RANKCLUE i = INTEGER          {Hanabi_types.RankClue i}

stepa:
  | s = step NEWLINE { s }

(* why it works : *)
(* files always end with a newline, and an EOF is inserted after that newline *)
record:
  | ht = my_nonempty_list(hand) s = list(stepa) EOF { fst ht, snd ht, s} 
