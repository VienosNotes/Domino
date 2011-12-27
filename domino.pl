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

class Parse {

#    BEGIN { Any.^add_method("at_key", method (*@_) { return Nil }); }

    method left ($/) {
        make '(';
    }

    method right ($/) {
        make ')';
    }

    method num ($/) {
        make $/.Str;
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
        if $<atom> {
            make $<atom>.ast;
        } else {
            make self!eval($<sexpr>);
        }
    }

    method TOP ($/) {
        make $<sexpr>.ast;
    }

    method !eval ($/) {
        say "called";
        return $/.Str;
    }
}

class Evaluate is Parse {

    has %!sym = {};

    method left ($/) {
        make '(';
    }

    method right ($/) {
        make ')';
    }

    method num ($/) {
        make $/.Str;
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
        if $<atom> {
            make $<atom>.ast;
        } else {
            make self!eval($<sexpr>);
        }
    }

    method TOP ($/) {
        make $<sexpr>.ast;
    }

    method !eval ($/) {
        say "override!";
        if $/[0]<atom><spform> -> $_ { return self!speval($_.ast, $/[1..*]) }
        elsif $/[0].ast ~~ %!sym { return self!funcval(%!sym{$/[0].ast}, $/[1..*]) }
        else { return "@(" ~ $/.map(*.ast).join(" ") ~ ")"; }
    }

    method !speval ($spform, $/) {
        given $spform {
            when "quote" {
                $/.elems == 1 or die "quote: wrong number of argument => " ~ $/.elems ~ "\n";
                return $/[0].ast.substr(1);
            }
            when "cdr" {
                my $m = Lisp.parse($/[0].ast, actions => Parse);
                self!check_elem($m<sexpr><sexpr>);
                return "(" ~ $m.<sexpr><sexpr>.map(*.ast)[1..*].join(" ") ~ ")";
            }
        }
    }

    method !funceval ($func, $/) {
        return "@(" ~ $/.map(*.ast).join(" ") ~ ")";
    }

    method !check_elem ($/) {
        none($/[0]>>.ast.match(/^\@\(/)).Bool or die "undefined funtion: " ~ $/[0]<sexpr>[0].ast ~ "\n";
    }

}


my $str = '(cdr (cdr (quote (1 2 3 4))))';
my $ev = Evaluate.new;
my $m = Lisp.parse($str, actions => $ev);
say "m = ",$m.ast;
