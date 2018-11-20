(https://en.wikipedia.org/wiki/Perl_language_structure) 読んでる

# Basic Syntax

- builtins: `print`, `say`, `use`
  - `use <versionname>;` という使い方もできるらしい
- `#` コメント (shebang も ok)

# Data Types

- 基本的なシジル: `$`, `@`, `%`, `&` (sub), `*` (型グロブ)

## Scalar Values
### Strings

- ダブルクォートないし `qq(...)`, `qq//` などで作る文字列リテラルは interpolate される (シジルから始まる式が展開される)
- シングルクォートないし `q()`, `q//` などで作る文字列リテラルはそのまま
- `\` で閉じクォートをエスケープできる

### Heredocs

```perl
my $heredoc = <<EOF
hogehoge
hogehoge
EOF
```

- `<<HOGEHOGE` と `HOGEHOGE` で囲まれた部分がヒアドキュメント (multiline string)

### Numbers

- builtins: `int`, `printf`, `sprintf`, `undef` 中置算術演算
- 整数 `10` (dec), `0b01` (bin), `0x01` (hex) など
- 小数 `0.1`, `1e-2`, `1E-3` など

## Array Values

- `qw()`, `qw//` などでベタ書きできる
- 変数に入れる時は `@hoge`
- 要素を一つだけ参照 `$hoge[1]`
- 要素をまとめて参照 `@hoge[1..2]`

## Hash Values

- `a => "b"` は `"a", "b"` と同じ
- 変数に入れる時は `%hoge`
- 要素を一つだけ参照 `$hoge{key}`
- 要素をまとめて参照 `@hoge{key, another_key}`

## Typeglob Values

- `*hoge` で `@hoge`, `%hoge`, `$hoge` などをまとめたシンボルテーブルエントリ取れる
- `*new_hoge = *hoge` のようにして名前をまるっとエイリアスしたいときとかに使う

## Array Functions

- 配列の最後の要素のインデックスを表すシジル `$#hoge`
- 代入すると配列の長さが変わったり (`$#hoge = 3` ... `@hoge` の長さが 4 になる)
- builtins: `scalar`

## Hash Functions

- builtins: `keys`, `values`

# Control Structures

- keywords: `while`, `continue`, `for`, `foreach`, `if`, `else`, `elsif`, `unless`, `until`, `next`, `last`, `return`, `redo`, `given`, `when`, `default`, `goto`, `sub`
- builtins: `and`, `or`, `grep`, `map`

# Subroutines

- 関数呼び出しはかっこ省略可
- builtins: `@_`, `shift`, `wantarray`
- 分割代入＆宣言 `my ($a, $b, $c) = (1, 2, 3)`
- 文字列の interpolation, `"hoge$_{1}"` とかも ok なので注意

# Regular Expressions
## Uses

- `m//` ないし `//` で正規表現
- `s///` で正規表現置換
- builtins: `split`

## Syntax

- 正規表現は複数行 ok
- 後ろにサフィックスがつく場合がある `//i` (ignore case), `s//g` (global replace), `//x` (コメントつき正規表現)
- builtins: `$1`, `$2`, ...

# Objects

- builtins: `bless`, `sqrt`,
- `sub Hoge::fuga { ... }` というのもできるらしい。 `Hoge->fuga` で呼べる
  - これは考慮したくない…
- `$$hoge[0]` というシンタックスがあるっぽい
  - `$hoge->[0]` と同じ意味っぽい？ (`@$hoge` の 0 番目 `$$hoge[0]`)
