%token TPLUS TMINUS TMULT TDIV TMOD TBITAND TBITOR TBITXOR TLSFT TRSFT TANOT
%token  TADDAS TSUBAS TMULAS TDIVAS TMODAS TANDAS TORAS TXORAS TLAS TRAS TANEQ
%token TAND TOR TREC TINC TEQ TLS TGR TASSIGN TNOT TNEQ TLSEQ TGREQ TCOLEQ
%token TTD TLPAR TRPAR TLBR TRBR TLCUR TRCUR TCOM TDOT TSEMCOL TCOL TRWSTR
%token<string> TSTR 
%token TRUNE
%token TEOF

%left TOR
%left TAND
%left TEQ TNEQ TLS TLSEQ TGR TGREQ
%left TPLUS TMINUS TBITOR TBITXOR
%left TMULT TDIV TMOD TLSFT TRSFT TBITAND TANOT 
%nonassoc UMINUS

%%
%%