use v6;


grammar Lisp {
    token left { '(' };
    token right { ')' };
    token num { \d+ }
    token str { '"'\w+'"' }
    token literal { <num>||<str> }
    token symbol { \w+ }
    token nil { 'nil' }
    token atom { <literal>||<nil>||<spform>||<symbol> }
    token spform { 'car' | 'cdr' | 'cons' | 'eq' | 'atom' | 'cond' | 'if' | 'define' | 'quote' }

    rule sexpr { <atom> || <left> <sexpr>+? % <.ws> <right> }
    rule TOP { ^^ <sexpr> $$ }
}

class Evaluate {

    BEGIN { Any.^add_method("at_key", method (*@_) { return Nil }); }

    method _spform () { return 'car' | 'cdr' | 'cons' | 'eq' | 'atom' | 'cond' | 'if' | 'define' | 'quote' }

    method left ($/) {
        make '(';
    }

    method right ($/) {
        make ')';
    }

    method num ($/) {
        make ($/.Int + 1).Str;
    }

    method str ($/) {
        make $/.Str;
    }

    method literal ($/) {
        make $<num>.?ast // $<str>.ast;
    }

    method symbol ($/) {
        make $/.Str;
    }

    method spform ($/) {
        make $/.Str;
    }

    method nil ($/) {
        make 'nil';
    }

    method atom ($/) {
        make $<nil>.?ast // $<spform>.?ast // $<literal>.?ast;
    }

    method sexpr ($/) {
        if $<sexpr>[0]<atom><spform> -> $_ {  };
        if $<atom> {
            make $<atom>.ast;
        } else {
            make '(' ~ $<sexpr>.map(*.ast).join(" ") ~ ')';
        }
    }

    method TOP ($/) {
        make $<sexpr>.ast;
    }

}

my $str = '(cdr 1 2 (car nil 1) (hoge 0 "abc"))';
my $m = Lisp.parse($str, actions => Evaluate);
say $m.ast;
