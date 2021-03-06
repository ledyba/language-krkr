;==============================================================================
; Copyright (c) 2010-2015 Bolero MURAKAMI
; https://github.com/bolero-MURAKAMI/KTL-Script
;
; Distributed under the Boost Software License, Version 1.0. (See accompanying
; file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
;==============================================================================
[call storage="ktl/ks/Scripts.ks"]
[guard storage="ktl/ks/Layer.ks"]

[iscript]
Sources.include("ktl/Iteration.tjs");
Sources.include("ktl/Utility.tjs");

//
// set_prop_layers
//
function set_prop_layers(param = %[]) {
	var page = valueOr(param.page, "fore");
	delete param.page;
	delete param.cond;
	for (var i = Iteration.begin(kag[page].layers); i.check(); i.next()) {
		for (var pi = Iteration.begin(param); pi.check(); pi.next()) {
			i.value[pi.name] = Scripts.eval(pi.value);
		}
	}
}
[endscript]

;
;	MACRO: set_prop_layers = すべてのKAGキャラクターレイヤのプロパティを設定
;	ATTRIBUTES:
;		OPTIONAL: page = 対象ページ
;		OPTIONAL: cond = 条件式
;		VARIADIC: ... = プロパティ
;
[macro name="set_prop_layers"]
[eval exp="set_prop_layers(mp)" cond=%cond|"true"]
[endmacro]

[return]
