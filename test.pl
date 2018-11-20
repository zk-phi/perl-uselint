package AwesomePackage {
    sub hoge { return "Hi, I'm AwesomePackage::hoge"; }
    sub hige { return "Hi, I'm AwesomePackage::hige"; }
}

package hoge::fuga {
    sub piyo { return "AwesomePackage"; }
};

package hoge::fuga::piyo {
    sub hoge { return "Hi, I'm hoge::fuga::piyo::hoge"; }
};

warn hoge::fuga::piyo->hoge; # hoge::fuga がパッケージ名、 piyo も hoge もサブルーチン
warn hoge::fuga::piyo::hige; # これは invalid
warn hoge::fuga::piyo::hoge; # hoge::fuga::piyo がパッケージ名、 hoge だけがサブルーチン

# ----------

package foo {
    sub bar { return "foo"; }
    sub baz { return "foo"; }
    sub qux { return "Hi, I'm foo::qux"; }
}

warn foo::bar::baz::qux;     # これも invalid だった
