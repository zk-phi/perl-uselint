use strict;
use warnings;
use utf8;

sub func {
    return "hoge";
}

my %test = (
    func => 2,
    hoge => 3
);

my $key = "func";

warn $test{func};               # 2
warn $test{func()};             # 3
warn $test{"fu" . "nc"};        # 2
warn $test{$key};               # 2

# シジルを伴わない１トークンの場合はリテラル、それ以外は式として解釈さ
# れる？
