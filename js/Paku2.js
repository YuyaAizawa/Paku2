!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,function(r){return function(t){return n(r,t)}})}function e(n){return r(3,n,function(r){return function(t){return function(e){return n(r,t,e)}}})}function u(n){return r(4,n,function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}})}function a(n){return r(5,n,function(r){return function(t){return function(e){return function(u){return function(a){return n(r,t,e,u,a)}}}}})}function i(n){return r(6,n,function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return n(r,t,e,u,a,i)}}}}}})}function c(n){return r(7,n,function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return function(c){return n(r,t,e,u,a,i,c)}}}}}}})}function f(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function v(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function s(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function b(n,r,t,e,u,a,i){return 6===n.a?n.f(r,t,e,u,a,i):n(r)(t)(e)(u)(a)(i)}function d(n,r,t,e,u,a,i,c){return 7===n.a?n.f(r,t,e,u,a,i,c):n(r)(t)(e)(u)(a)(i)(c)}var l=e(function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e}),$=t(function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,E(t,r)}),h={$:0};function g(n,r){return{$:1,a:n,b:r}}var p=t(g);function m(n){for(var r=h,t=n.length;t--;)r=g(n[t],r);return r}function w(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var y=e(function(n,r,t){for(var e=[];r.b&&t.b;r=r.b,t=t.b)e.push(f(n,r.a,t.a));return m(e)});function F(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}function j(n,r){for(var t,e=[],u=A(n,r,0,e);u&&(t=e.pop());u=A(t.a,t.b,0,e));return u}function A(n,r,t,e){if(t>100)return e.push(E(n,r)),!0;if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&F(5),!1;for(var u in 0>n.$&&(n=_r(n),r=_r(r)),n)if(!A(n[u],r[u],t+1,e))return!1;return!0}function W(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=W(n.a,r.a))?t:(t=W(n.b,r.b))?t:W(n.c,r.c);for(;n.b&&r.b&&!(t=W(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var k=t(function(n,r){var t=W(n,r);return 0>t?jr:t?Wr:Fr}),_=0;function E(n,r){return{a:n,b:r}}function N(n,r,t){return{a:n,b:r,c:t}}function L(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function T(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=g(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=g(n.a,r);return t}function x(n){return{$:0,a:n}}function q(n){return{$:2,b:n,c:null}}var C=t(function(n,r){return{$:3,b:n,d:r}});var B=0;function I(n){var r={$:0,e:B++,f:n,g:null,h:[]};return z(r),r}function D(n){return q(function(r){r(x(I(n)))})}function R(n,r){n.h.push(r),z(n)}var O=t(function(n,r){return q(function(t){R(n,r),t(x(_))})});var S=!1,Y=[];function z(n){if(Y.push(n),!S){for(S=!0;n=Y.shift();)G(n);S=!1}}function G(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b(function(r){n.f=r,z(n)}));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var P=Math.ceil,K=Math.floor,Z=Math.log;var M=t(function(n,r){return r.split(n)}),X=t(function(n,r){return r.join(n)});function J(n){return{$:2,b:n}}var U=J(function(n){return"number"!=typeof n?dn("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?ot(n):!isFinite(n)||n%1?dn("an INT",n):ot(n)}),H=J(function(n){return"boolean"==typeof n?ot(n):dn("a BOOL",n)}),Q=J(function(n){return"number"==typeof n?ot(n):dn("a FLOAT",n)}),V=(J(function(n){return ot(hn(n))}),J(function(n){return"string"==typeof n?ot(n):n instanceof String?ot(n+""):dn("a STRING",n)}));var nn=t(function(n,r){return{$:6,d:n,b:r}});function rn(n,r){return{$:9,f:n,g:r}}var tn=t(function(n,r){return{$:10,b:r,h:n}});var en=t(function(n,r){return rn(n,[r])}),un=e(function(n,r,t){return rn(n,[r,t])}),an=u(function(n,r,t,e){return rn(n,[r,t,e])}),cn=a(function(n,r,t,e,u){return rn(n,[r,t,e,u])}),fn=t(function(n,r){return on(n,gn(r))});function on(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?ot(n.c):dn("null",r);case 3:return sn(r)?vn(n.b,r,m):dn("a LIST",r);case 4:return sn(r)?vn(n.b,r,bn):dn("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return dn("an OBJECT with a field named `"+t+"`",r);var e=on(n.b,r[t]);return Pr(e)?e:ft(f(st,t,e.a));case 7:var u=n.e;if(!sn(r))return dn("an ARRAY",r);if(u>=r.length)return dn("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=on(n.b,r[u]);return Pr(e)?e:ft(f(bt,u,e.a));case 8:if("object"!=typeof r||null===r||sn(r))return dn("an OBJECT",r);var a=h;for(var i in r)if(r.hasOwnProperty(i)){e=on(n.b,r[i]);if(!Pr(e))return ft(f(st,i,e.a));a=g(E(i,e.a),a)}return ot(Ir(a));case 9:for(var c=n.f,o=n.g,v=0;o.length>v;v++){e=on(o[v],r);if(!Pr(e))return e;c=c(e.a)}return ot(c);case 10:e=on(n.b,r);return Pr(e)?on(n.h(e.a),r):e;case 11:for(var s=h,b=n.g;b.b;b=b.b){e=on(b.a,r);if(Pr(e))return e;s=g(e.a,s)}return ft(dt(Ir(s)));case 1:return ft(f(vt,n.a,hn(r)));case 0:return ot(n.a)}}function vn(n,r,t){for(var e=r.length,u=Array(e),a=0;e>a;a++){var i=on(n,r[a]);if(!Pr(i))return ft(f(bt,a,i.a));u[a]=i.a}return ot(t(u))}function sn(n){return Array.isArray(n)||"function"==typeof FileList&&n instanceof FileList}function bn(n){return f(it,n.length,function(r){return n[r]})}function dn(n,r){return ft(f(vt,"Expecting "+n,hn(r)))}function ln(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return ln(n.b,r.b);case 6:return n.d===r.d&&ln(n.b,r.b);case 7:return n.e===r.e&&ln(n.b,r.b);case 9:return n.f===r.f&&$n(n.g,r.g);case 10:return n.h===r.h&&ln(n.b,r.b);case 11:return $n(n.g,r.g)}}function $n(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!ln(n[e],r[e]))return!1;return!0}function hn(n){return n}function gn(n){return n}hn(null);function pn(n,r,t,e,u,a){var i=f(fn,n,hn(r?r.flags:void 0));Pr(i)||F(2);var c={},o=(i=t(i.a)).a,v=a(b,o),s=function(n,r){var t;for(var e in mn){var u=mn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=yn(u,r)}return t}(c,b);function b(n,r){v(o=(i=f(e,n,o)).a,r),kn(c,i.b,u(o))}return kn(c,i.b,u(o)),s?{ports:s}:{}}var mn={};function wn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function yn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,a=n.e,i=n.f;return t.h=I(f(C,function n(r){return f(C,n,{$:5,b:function(n){var c=n.a;return 0===n.$?o(u,t,c,r):a&&i?v(e,t,c.i,c.j,r):o(e,t,a?c.i:c.j,r)}})},n.b))}var Fn=t(function(n,r){return q(function(t){n.g(r),t(x(_))})}),jn=t(function(n,r){return f(O,n.h,{$:0,a:r})});function An(n){return function(r){return{$:1,k:n,l:r}}}function Wn(n){return{$:2,m:n}}function kn(n,r,t){var e={};for(var u in _n(!0,r,e,null),_n(!1,t,e,null),n)R(n[u],{$:"fx",a:e[u]||{i:h,j:h}})}function _n(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,r,t,e){return f(n?mn[r].e:mn[r].f,function(n){for(var r=t;r;r=r.q)n=r.p(n);return n},e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:h,j:h},n?t.i=g(r,t.i):t.j=g(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)_n(n,i.a,t,e);return;case 3:return void _n(n,r.o,t,{p:r.n,q:e})}}var En;var Nn="undefined"!=typeof document?document:{};function Ln(n,r){n.appendChild(r)}function Tn(n){return{$:0,a:n}}var xn=t(function(n,r){return t(function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:r,d:Rn(t),e:u,f:n,b:a}})}),qn=xn(void 0);t(function(n,r){return t(function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:r,d:Rn(t),e:u,f:n,b:a}})})(void 0);var Cn=t(function(n,r){return{$:"a0",n:n,o:r}}),Bn=t(function(n,r){return{$:"a2",n:n,o:r}}),In=t(function(n,r){return{$:"a3",n:n,o:r}});var Dn;function Rn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?On(i,u,a):i[u]=a}else"className"===u?On(r,u,gn(a)):r[u]=gn(a)}return r}function On(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Sn(n,r){var t=n.$;if(5===t)return Sn(n.k||(n.k=n.m()),r);if(0===t)return Nn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=Sn(e,a)).elm_event_node_ref=a,i}if(3===t)return Yn(i=n.h(n.g),r,n.d),i;var i=n.f?Nn.createElementNS(n.f,n.c):Nn.createElement(n.c);En&&"a"==n.c&&i.addEventListener("click",En(i)),Yn(i,r,n.d);for(var c=n.e,f=0;c.length>f;f++)Ln(i,Sn(1===t?c[f]:c[f].b,r));return i}function Yn(n,r,t){for(var e in t){var u=t[e];"a1"===e?zn(n,u):"a0"===e?Kn(n,r,u):"a3"===e?Gn(n,u):"a4"===e?Pn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function zn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Gn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Pn(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;void 0!==a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function Kn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=Zn(r,a),n.addEventListener(u,i,Dn&&{passive:2>Yt(a)}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Dn=!0}}))}catch(n){}function Zn(n,r){function t(r){var e=t.q,u=on(e.a,r);if(Pr(u)){for(var a,i=Yt(e),c=u.a,f=i?3>i?c.a:c.n:c,o=1==i?c.b:3==i&&c.aa,v=(o&&r.stopPropagation(),(2==i?c.b:3==i&&c.Z)&&r.preventDefault(),n);a=v.j;){if("function"==typeof a)f=a(f);else for(var s=a.length;s--;)f=a[s](f);v=v.p}v(f,o)}}return t.q=r,t}function Mn(n,r){return n.$==r.$&&ln(n.a,r.a)}function Xn(n,r){var t=[];return Un(n,r,t,0),t}function Jn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Un(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void Jn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,c=r.l,f=i.length,o=f===c.length;o&&f--;)o=i[f]===c[f];if(o)return void(r.k=n.k);r.k=r.m();var v=[];return Un(n.k,r.k,v,0),void(v.length>0&&Jn(t,1,e,v));case 4:for(var s=n.j,b=r.j,d=!1,l=n.k;4===l.$;)d=!0,"object"!=typeof s?s=[s,l.j]:s.push(l.j),l=l.k;for(var $=r.k;4===$.$;)d=!0,"object"!=typeof b?b=[b,$.j]:b.push($.j),$=$.k;return d&&s.length!==b.length?void Jn(t,0,e,r):((d?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Jn(t,2,e,b),void Un(l,$,t,e+1));case 0:return void(n.a!==r.a&&Jn(t,3,e,r.a));case 1:return void Hn(n,r,t,e,Vn);case 2:return void Hn(n,r,t,e,nr);case 3:if(n.h!==r.h)return void Jn(t,0,e,r);var h=Qn(n.d,r.d);h&&Jn(t,4,e,h);var g=r.i(n.g,r.g);return void(g&&Jn(t,5,e,g))}}}function Hn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=Qn(n.d,r.d);a&&Jn(t,4,e,a),u(n,r,t,e)}else Jn(t,0,e,r)}function Qn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&Mn(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var c=Qn(n[u],r[u]||{},u);c&&((e=e||{})[u]=c)}for(var f in r)f in n||((e=e||{})[f]=r[f]);return e}function Vn(n,r,t,e){var u=n.e,a=r.e,i=u.length,c=a.length;i>c?Jn(t,6,e,{v:c,i:i-c}):c>i&&Jn(t,7,e,{v:i,e:a});for(var f=c>i?i:c,o=0;f>o;o++){var v=u[o];Un(v,a[o],t,++e),e+=v.b||0}}function nr(n,r,t,e){for(var u=[],a={},i=[],c=n.e,f=r.e,o=c.length,v=f.length,s=0,b=0,d=e;o>s&&v>b;){var l=(k=c[s]).a,$=(_=f[b]).a,h=k.b,g=_.b,p=void 0,m=void 0;if(l!==$){var w=c[s+1],y=f[b+1];if(w){var F=w.a,j=w.b;m=$===F}if(y){var A=y.a,W=y.b;p=l===A}if(p&&m)Un(h,W,u,++d),tr(a,u,l,g,b,i),d+=h.b||0,er(a,u,l,j,++d),d+=j.b||0,s+=2,b+=2;else if(p)d++,tr(a,u,$,g,b,i),Un(h,W,u,d),d+=h.b||0,s+=1,b+=2;else if(m)er(a,u,l,h,++d),d+=h.b||0,Un(j,g,u,++d),d+=j.b||0,s+=2,b+=1;else{if(!w||F!==A)break;er(a,u,l,h,++d),tr(a,u,$,g,b,i),d+=h.b||0,Un(j,W,u,++d),d+=j.b||0,s+=2,b+=2}}else Un(h,g,u,++d),d+=h.b||0,s++,b++}for(;o>s;){var k;er(a,u,(k=c[s]).a,h=k.b,++d),d+=h.b||0,s++}for(;v>b;){var _,E=E||[];tr(a,u,(_=f[b]).a,_.b,void 0,E),b++}(u.length>0||i.length>0||E)&&Jn(t,8,e,{w:u,x:i,y:E})}var rr="_elmW6BL";function tr(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var c=[];return Un(i.z,e,c,i.r),i.r=u,void(i.s.s={w:c,A:i})}tr(n,r,t+rr,e,u,a)}function er(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return Un(e,a.z,i,u),void Jn(r,9,u,{w:i,A:a})}er(n,r,t+rr,e,u)}else{var c=Jn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:c}}}function ur(n,r,t,e){!function n(r,t,e,u,a,i,c){var f=e[u];var o=f.r;for(;o===a;){var v=f.$;if(1===v)ur(r,t.k,f.s,c);else if(8===v){f.t=r,f.u=c;var s=f.s.w;s.length>0&&n(r,t,s,0,a,i,c)}else if(9===v){f.t=r,f.u=c;var b=f.s;if(b){b.A.s=r;var s=b.w;s.length>0&&n(r,t,s,0,a,i,c)}}else f.t=r,f.u=c;if(!(f=e[++u])||(o=f.r)>i)return u}var d=t.$;if(4===d){for(var l=t.k;4===l.$;)l=l.k;return n(r,l,e,u,a+1,i,r.elm_event_node_ref)}var $=t.e;var h=r.childNodes;for(var g=0;$.length>g;g++){var p=1===d?$[g]:$[g].b,m=++a+(p.b||0);if(o>=a&&m>=o&&(u=n(h[g],p,e,u,a,m,c),!(f=e[u])||(o=f.r)>i))return u;a=m}return u}(n,r,t,0,0,r.b,e)}function ar(n,r,t,e){return 0===t.length?n:(ur(n,r,t,e),ir(n,t))}function ir(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,a=cr(u,e);u===n&&(n=a)}return n}function cr(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Sn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Yn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return ir(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Sn(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=ir(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=Nn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e],a=u.A;Ln(t,2===a.c?a.s:Sn(a.z,r.u))}return t}(t.y,r);n=ir(n,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],c=i.A,f=2===c.c?c.s:Sn(c.z,r.u);n.insertBefore(f,n.childNodes[i.r])}e&&Ln(n,e);return n}(n,r);case 5:return r.s(n);default:F(10)}}function fr(n){if(3===n.nodeType)return Tn(n.textContent);if(1!==n.nodeType)return Tn("");for(var r=h,t=n.attributes,e=t.length;e--;){var u=t[e];r=g(f(In,u.name,u.value),r)}var a=n.tagName.toLowerCase(),i=h,c=n.childNodes;for(e=c.length;e--;)i=g(fr(c[e]),i);return o(qn,a,r,i)}var or=u(function(n,r,t,e){return pn(r,e,n.aY,n.bj,n.bd,function(r,t){var u=n.bl,a=e.node,i=fr(a);return sr(t,function(n){var t=u(n),e=Xn(i,t);a=ar(a,i,e,r),i=t})})}),vr=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function sr(n,r){r(n);var t=0;function e(){t=1===t?0:(vr(e),r(n),1)}return function(u,a){n=u,a?(r(n),2===t&&(t=1)):(0===t&&vr(e),t=2)}}var br={addEventListener:function(){},removeEventListener:function(){}},dr="undefined"!=typeof document?document:br,lr="undefined"!=typeof window?window:br,$r=e(function(n,r,t){return D(q(function(){function e(n){I(t(n))}return n.addEventListener(r,e,Dn&&{passive:!0}),function(){n.removeEventListener(r,e)}}))}),hr=t(function(n,r){var t=on(n,r);return Pr(t)?ct(t.a):Lr});var gr=t(function(n,r){return q(function(){var t=setInterval(function(){I(r)},n);return function(){clearInterval(t)}})});var pr={$:6},mr={$:0},wr={$:-2},yr=wr,Fr=1,jr=0,Ar=p,Wr=2,kr=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=o(n,t.b,t.c,o(kr,n,r,t.e));n=u,r=a,t=e}}),_r=function(n){return o(kr,e(function(n,r,t){return f(Ar,E(n,r),t)}),h,n)},Er=function(n){return o(kr,e(function(n,r,t){return f(Ar,n,t)}),h,n)},Nr={u:0,j:yr,o:!1,q:E(0,0),D:E(0,0)},Lr={$:1},Tr={s:Lr,I:0,i:mr,e:Nr,x:"WWWWWWWWW\nW   ,  +W\nW  , C2,W\nW    ;  W\nWG,     W\nW B   ,GW\nWWWWWWWWW"},xr=function(n){return n},qr=x,Cr=qr(0),Br=e(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=f(n,t.a,r);n=u,r=a,t=e}}),Ir=function(n){return o(Br,Ar,h,n)},Dr=u(function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,c=a.b;if(c.b){var s=c.a,b=c.b;if(b.b){var d=b.b;return f(n,u,f(n,i,f(n,s,f(n,b.a,t>500?o(Br,n,r,Ir(d)):v(Dr,n,r,t+1,d)))))}return f(n,u,f(n,i,f(n,s,r)))}return f(n,u,f(n,i,r))}return f(n,u,r)}return r}),Rr=e(function(n,r,t){return v(Dr,n,r,0,t)}),Or=t(function(n,r){return o(Rr,t(function(r,t){return f(Ar,n(r),t)}),h,r)}),Sr=C,Yr=t(function(n,r){return f(Sr,function(r){return qr(n(r))},r)}),zr=e(function(n,r,t){return f(Sr,function(r){return f(Sr,function(t){return qr(f(n,r,t))},t)},r)}),Gr=function(n){return o(Rr,zr(Ar),qr(h),n)},Pr=function(n){return!n.$},Kr=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),Zr=P,Mr=t(function(n,r){return Z(r)/Z(n)}),Xr=Zr(f(Mr,2,32)),Jr=[],Ur=v(Kr,0,Xr,Jr,Jr),Hr=$,Qr=t(function(n,r){for(;;){var t=f(Hr,32,n),e=t.b,u=f(Ar,{$:0,a:t.a},r);if(!e.b)return Ir(u);n=e,r=u}}),Vr=t(function(n,r){for(;;){var t=Zr(r/32);if(1===t)return f(Hr,32,n).a;n=f(Qr,n,h),r=t}}),nt=K,rt=t(function(n,r){return W(n,r)>0?n:r}),tt=function(n){return n.length},et=t(function(n,r){if(r.a){var t=32*r.a,e=nt(f(Mr,32,t-1)),u=n?Ir(r.d):r.d,a=f(Vr,u,r.a);return v(Kr,tt(r.c)+t,f(rt,5,e*Xr),a,r.c)}return v(Kr,tt(r.c),Xr,Jr,r.c)}),ut=l,at=a(function(n,r,t,e,u){for(;;){if(0>r)return f(et,!1,{d:e,a:t/32|0,c:u});var a={$:1,a:o(ut,32,r,n)};n=n,r=r-32,t=t,e=f(Ar,a,e),u=u}}),it=t(function(n,r){if(n>0){var t=n%32;return s(at,r,n-t-32,n,h,o(ut,t,n-t,r))}return Ur}),ct=function(n){return{$:0,a:n}},ft=function(n){return{$:1,a:n}},ot=function(n){return{$:0,a:n}},vt=t(function(n,r){return{$:3,a:n,b:r}}),st=t(function(n,r){return{$:0,a:n,b:r}}),bt=t(function(n,r){return{$:1,a:n,b:r}}),dt=function(n){return{$:2,a:n}},lt=function(n){return o(Br,t(function(n,r){return r+1}),0,n)},$t=y,ht=e(function(n,r,t){for(;;){if(W(n,r)>=1)return t;var e=n,u=r-1,a=f(Ar,r,t);n=e,r=u,t=a}}),gt=t(function(n,r){return o(ht,n,r,h)}),pt=t(function(n,r){return o($t,n,f(gt,0,lt(r)-1),r)}),mt=function(n){return n+""},wt=t(function(n,r){return f(X,n,w(r))}),yt=t(function(n,r){return m(f(M,n,r))}),Ft=Fn,jt=t(function(n,r){var t=r;return D(f(Sr,Ft(n),t))});mn.Task=wn(Cr,e(function(n,r){return f(Yr,function(){return 0},Gr(f(Or,jt(n),r)))}),e(function(){return qr(0)}),t(function(n,r){return f(Yr,n,r)}));var At=An("Task"),Wt=t(function(n,r){return At(f(Yr,n,r))}),kt={$:1},_t=function(n){return{$:2,a:n}},Et={$:0},Nt=nn,Lt=en,Tt=V,xt=f(Lt,function(n){switch(n){case"ArrowUp":return _t(0);case"ArrowDown":return _t(1);case"ArrowRight":return _t(3);case"ArrowLeft":return _t(2);default:return Et}},f(Nt,"key",Tt)),qt=e(function(n,r,t){return{$:0,a:n,b:r,c:t}}),Ct=t(function(n,r){return{as:r,aB:n}}),Bt=qr(f(Ct,h,yr)),It=function(n){var r=n.b;return E(T(function(n){return n?"w_":"d_"}(n.a),r),n)},Dt=t(function(n,r){return{ag:r,aj:n}}),Rt=jn,Ot=un,St=function(n){return{$:0,a:n}},Yt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},zt=e(function(n,r,t){return f(Yr,function(n){return E(r,n)},o($r,t.a?lr:dr,t.b,function(t){return f(Rt,n,f(Dt,r,t))}))}),Gt=a(function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}}),Pt=k,Kt=a(function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return s(Gt,n,r,t,e,u);var a=e.d;v=e.e;return s(Gt,0,e.b,e.c,s(Gt,1,a.b,a.c,a.d,a.e),s(Gt,1,r,t,v,u))}var i=u.b,c=u.c,f=u.d,o=u.e;if(-1!==e.$||e.a)return s(Gt,n,i,c,s(Gt,0,r,t,e,f),o);var v;return s(Gt,0,r,t,s(Gt,1,e.b,e.c,e.d,v=e.e),s(Gt,1,i,c,f,o))}),Zt=e(function(n,r,t){if(-2===t.$)return s(Gt,0,n,r,wr,wr);var e=t.a,u=t.b,a=t.c,i=t.d,c=t.e;switch(f(Pt,n,u)){case 0:return s(Kt,e,u,a,o(Zt,n,r,i),c);case 1:return s(Gt,e,u,r,i,c);default:return s(Kt,e,u,a,i,o(Zt,n,r,c))}}),Mt=e(function(n,r,t){var e=o(Zt,n,r,t);if(-1!==e.$||e.a)return e;return s(Gt,1,e.b,e.c,e.d,e.e)}),Xt=function(n){return o(Br,t(function(n,r){return o(Mt,n.a,n.b,r)}),yr,n)},Jt=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,a=o(n,t.b,t.c,o(Jt,n,r,t.d));n=u,r=a,t=e}}),Ut=i(function(n,r,u,a,i,c){var f=o(Jt,e(function(t,e,a){n:for(;;){var i=a.a,c=a.b;if(i.b){var f=i.a,s=f.a,b=f.b,d=i.b;if(0>W(s,t)){t=t,e=e,a=E(d,o(n,s,b,c));continue n}return W(s,t)>0?E(i,o(u,t,e,c)):E(d,v(r,s,b,e,c))}return E(i,o(u,t,e,c))}}),E(_r(a),c),i),s=f.a,b=f.b;return o(Br,t(function(r,t){return o(n,r.a,r.b,t)}),b,s)}),Ht=t(function(n,r){return o(Jt,Mt,r,n)}),Qt=function(n){return q(function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(x(_))})},Vt=e(function(n,r,t){var a=e(function(r,t,e){var u=e.c;return N(e.a,e.b,f(Ar,o(zt,n,r,t),u))}),i=e(function(n,r,t){var e=t.b,u=t.c;return N(f(Ar,r,t.a),e,u)}),c=u(function(n,r,t,e){var u=e.c;return N(e.a,o(Mt,n,r,e.b),u)}),v=f(Or,It,r),s=b(Ut,i,c,a,t.as,Xt(v),N(h,yr,h)),d=s.b,l=s.c;return f(Sr,function(n){return qr(f(Ct,v,f(Ht,d,Xt(n))))},f(Sr,function(){return Gr(l)},Gr(f(Or,Qt,s.a))))}),ne=e(function(n,r,t){var e=n(r);return e.$?t:f(Ar,e.a,t)}),re=t(function(n,r){return o(Rr,ne(n),h,r)});mn["Browser.Events"]=wn(Bt,Vt,e(function(n,r,t){var e=r.aj,u=r.ag,a=f(re,function(n){var r=n.b,t=r.c;return j(n.a,e)?f(hr,t,u):Lr},t.aB);return f(Sr,function(){return qr(t)},Gr(f(Or,Ft(n),a)))}),0,t(function(n,r){return o(qt,r.a,r.b,f(Lt,n,r.c))}));var te,ee=An("Browser.Events"),ue=f(e(function(n,r,t){return ee(o(qt,n,r,t))}),0,"keydown"),ae=Wn,ie=t(function(n,r){return{$:0,a:n,b:r}}),ce=t(function(n,r){return{av:r,aC:n}}),fe=qr(f(ce,yr,yr)),oe=t(function(n,r){n:for(;;){if(-2===r.$)return Lr;var t=r.c,e=r.d,u=r.e;switch(f(Pt,n,r.b)){case 0:n=n,r=e;continue n;case 1:return ct(t);default:n=n,r=u;continue n}}}),ve=t(function(n,r){var t=n.a,e=n.b,u=f(oe,t,r);return o(Mt,t,1===u.$?m([e]):f(Ar,e,u.a),r)}),se=D,be=gr,de=e(function(n,r,t){if(r.b){var e=r.a,u=r.b,a=se(f(be,e,f(Rt,n,e)));return f(Sr,function(r){return o(de,n,u,o(Mt,e,r,t))},a)}return qr(t)}),le=e(function(n,r,t){var a=t.av,i=e(function(n,r,t){var e=t.c;return N(t.a,t.b,f(Sr,function(){return e},Qt(r)))}),c=o(Br,ve,yr,r),v=b(Ut,e(function(n,r,t){var e=t.b,u=t.c;return N(f(Ar,n,t.a),e,u)}),u(function(n,r,t,e){var u=e.c;return N(e.a,o(Mt,n,t,e.b),u)}),i,c,a,N(h,yr,qr(0))),s=v.a,d=v.b;return f(Sr,function(n){return qr(f(ce,c,n))},f(Sr,function(){return o(de,n,s,d)},v.c))}),$e=(te=xr,q(function(n){n(x(te(Date.now())))})),he=e(function(n,r,t){var e=f(oe,r,t.aC);if(1===e.$)return qr(t);var u=e.a;return f(Sr,function(){return qr(t)},f(Sr,function(r){return Gr(f(Or,function(t){return f(Ft,n,t(r))},u))},$e))}),ge=e(function(n,r,t){return n(r(t))});mn.Time=wn(fe,le,he,0,t(function(n,r){return f(ie,r.a,f(ge,n,r.b))}));var pe=An("Time"),me=t(function(n,r){return pe(f(ie,n,r))}),we={$:1},ye=t(function(n,r){return{$:2,a:n,b:r}}),Fe=function(n){return{$:4,a:n}},je=function(n){return{$:8,a:n}},Ae=t(function(n,r){return{$:0,a:n,b:r}}),We=function(n){var r=n.b;return f(Ae,1664525*n.a+r>>>0,r)},ke=function(n){var r=n.a,t=277803737*(r^r>>>4+(r>>>28));return(t>>>22^t)>>>0},_e=t(function(n,r){return function(t){var e=0>W(n,r)?E(n,r):E(r,n),u=e.a,a=e.b-u+1;if(a-1&a){var i=(-a>>>0)%a>>>0;return function(n){for(;;){var r=ke(n),t=We(n);if(W(r,i)>=0)return E(r%a+u,t);n=t}}(t)}return E(((a-1&ke(t))>>>0)+u,We(t))}}),Ee=t(function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return E(n(e.a),u)}}),Ne=f(Ee,function(n){switch(n){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},f(_e,0,3)),Le=e(function(n,r,t){var e=r,u=t;return function(r){var t=e(r),a=t.a,i=u(t.b),c=i.b;return E(f(n,a,i.a),c)}}),Te=f(t(function(n,r){return o(Le,t(function(n,r){return E(n,r)}),n,r)}),Ne,f(_e,5,10)),xe=t(function(n,r){var t=r.a,e=r.b;switch(n){case 0:return E(t,e-1);case 1:return E(t,e+1);case 2:return E(t-1,e);default:return E(t+1,e)}}),qe=function(n){if(-1===n.$&&-1===n.d.$&&-1===n.e.$){if(-1!==n.e.d.$||n.e.d.a){var r=n.d,t=n.e;i=t.b,c=t.c,e=t.d,v=t.e;return s(Gt,1,n.b,n.c,s(Gt,0,r.b,r.c,r.d,r.e),s(Gt,0,i,c,e,v))}var e,u=n.d,a=n.e,i=a.b,c=a.c,f=(e=a.d,e.d),o=e.e,v=a.e;return s(Gt,0,e.b,e.c,s(Gt,1,n.b,n.c,s(Gt,0,u.b,u.c,u.d,u.e),f),s(Gt,1,i,c,o,v))}return n},Ce=function(n){if(-1===n.$&&-1===n.d.$&&-1===n.e.$){if(-1!==n.d.d.$||n.d.d.a){var r=n.d,t=r.d,e=n.e;v=e.b,b=e.c,d=e.d,l=e.e;return s(Gt,1,u=n.b,a=n.c,s(Gt,0,r.b,r.c,t,f=r.e),s(Gt,0,v,b,d,l))}var u=n.b,a=n.c,i=n.d,c=i.d,f=i.e,o=n.e,v=o.b,b=o.c,d=o.d,l=o.e;return s(Gt,0,i.b,i.c,s(Gt,1,c.b,c.c,c.d,c.e),s(Gt,1,u,a,f,s(Gt,0,v,b,d,l)))}return n},Be=c(function(n,r,t,e,u,a,i){if(-1!==a.$||a.a){n:for(;;){if(-1===i.$&&1===i.a){if(-1===i.d.$){if(1===i.d.a){return Ce(r)}break n}return Ce(r)}break n}return r}return s(Gt,t,a.b,a.c,a.d,s(Gt,0,e,u,a.e,i))}),Ie=function(n){if(-1===n.$&&-1===n.d.$){var r=n.a,t=n.b,e=n.c,u=n.d,a=u.d,i=n.e;if(1===u.a){if(-1!==a.$||a.a){var c=qe(n);if(-1===c.$){var f=c.e;return s(Kt,c.a,c.b,c.c,Ie(c.d),f)}return wr}return s(Gt,r,t,e,Ie(u),i)}return s(Gt,r,t,e,Ie(u),i)}return wr},De=t(function(n,r){if(-2===r.$)return wr;var t=r.a,e=r.b,u=r.c,a=r.d,i=r.e;if(0>W(n,e)){if(-1===a.$&&1===a.a){var c=a.d;if(-1!==c.$||c.a){var o=qe(r);if(-1===o.$){var v=o.e;return s(Kt,o.a,o.b,o.c,f(De,n,o.d),v)}return wr}return s(Gt,t,e,u,f(De,n,a),i)}return s(Gt,t,e,u,f(De,n,a),i)}return f(Re,n,d(Be,n,r,t,e,u,a,i))}),Re=t(function(n,r){if(-1===r.$){var t=r.a,e=r.b,u=r.c,a=r.d,i=r.e;if(j(n,e)){var c=function(n){for(;;){if(-1!==n.$||-1!==n.d.$)return n;n=n.d}}(i);return-1===c.$?s(Kt,t,c.b,c.c,a,Ie(i)):wr}return s(Kt,t,e,u,a,f(De,n,i))}return wr}),Oe=t(function(n,r){var t=f(De,n,r);if(-1!==t.$||t.a)return t;return s(Gt,1,t.b,t.c,t.d,t.e)}),Se=t(function(n,r){return r.$?Lr:ct(n(r.a))}),Ye=t(function(n,r){return r.$?n:r.a}),ze=e(function(n,r,t){var e=f(xe,r,n);return f(Ye,t,f(Se,function(r){return o(Mt,e,r,f(Oe,n,t))},f(oe,n,t)))}),Ge=u(function(n,r,t,e){return W((t-n)*(t-n),(e-r)*(e-r))>0?t-n>0?3:2:e-r>0?1:0}),Pe=t(function(n,r){return n(r)}),Ke=e(function(n,r,t){var e=n.a,u=n.b,a=r.a,i=r.b,c=f(Pe,f(_e,0,3),t),o=c.a,s=c.b,b=function(){switch(o){case 0:return E(ct(v(Ge,e,u,a,i)),s);case 1:return r=(n=f(Pe,Ne,s)).b,E(ct(n.a),r);default:return E(Lr,s)}var n,r}();return E(b.a,b.b)}),Ze=u(function(n,r,t,e){var u=e.a,a=e.b,i=e.c;switch(r.$){case 4:var c=r.a;return N(function(){var r=f(oe,f(xe,c,n),u);if(1===r.$)return o(ze,n,c,u);switch(r.a.$){case 5:return o(Mt,n,Fe(function(n){switch(n){case 0:return 3;case 1:return 2;case 2:return 0;default:return 1}}(c)),u);case 6:return o(Mt,n,Fe(function(n){switch(n){case 0:return 2;case 1:return 3;case 2:return 1;default:return 0}}(c)),u);default:return u}}(),a,i);case 2:if(r.b)return N(o(Mt,n,f(ye,r.a,r.b-1),u),a,i);var v=f(Pe,Te,i),s=v.a,b=v.b;return N(o(Mt,n,f(ye,s.a,s.b),u),a,b);case 8:var d=r.a,l=f(Oe,n,u),$=je(2===d?0:d+1),h=o(Ke,n,t,i),g=(b=h.b,f(Ye,n,f(Se,function(r){return f(xe,r,n)},h.a))),p=function(){var r=f(oe,g,l);if(1===r.$)return E(o(Mt,g,$,l),a);if(r.a.$)return E(o(Mt,n,$,l),a);return E(o(Mt,n,$,l),!0)}();return N(p.a,p.b,b);default:return N(u,a,i)}}),Me=t(function(n,r){var t=o(Rr,function(n){return o(Ze,n.a,n.b,r.q)},N(r.j,r.o,n),_r(r.j));return L(r,{j:t.a,o:t.b})}),Xe=t(function(n,r){var t=r.x;return{s:r.s,I:r.I+1,i:mr,e:f(Me,n,r.e),x:t}}),Je=function(n){var r=We(f(Ae,0,1013904223));return We(f(Ae,r.a+n>>>0,r.b))},Ue=f(Sr,function(n){return qr(Je(n))},$e),He=e(function(n,r,t){if(r.b){var e=r.b,u=f(Pe,r.a,t),a=u.b;return f(Sr,function(){return o(He,n,e,a)},f(Ft,n,u.a))}return qr(t)});mn.Random=wn(Ue,He,e(function(n,r,t){return qr(t)}),t(function(n,r){return f(Ee,n,r)}));var Qe,Ve=An("Random"),nu=f(t(function(n,r){return Ve(f(Ee,n,r))}),function(n){return{$:4,a:n}},f(_e,-2147483648,2147483647)),ru={$:6},tu={$:3},eu={$:5},uu={$:7},au={$:0},iu={$:1},cu=t(function(n,r){return r.b?o(Rr,Ar,r,n):n}),fu=t(function(n,r){return o(Rr,cu,h,f(Or,n,r))}),ou=t(function(n,r){return o(Rr,t(function(r,t){return n(r)?f(Ar,r,t):t}),h,r)}),vu=function(n){return n.b?ct(o(Br,rt,n.a,n.b)):Lr},su=function(n){var r,t=Xt(f(fu,function(n){return n},f(pt,function(n){return function(r){return f(re,function(n){return n},f(pt,function(r){return function(t){return f(Se,function(t){return E(E(r,n),t)},function(n){switch(n){case"W":return ct(iu);case"G":return ct(f(ye,0,0));case"B":return ct(tu);case"8":return ct(Fe(0));case"2":return ct(Fe(1));case"4":return ct(Fe(2));case"6":return ct(Fe(3));case",":return ct(eu);case";":return ct(ru);case"C":return ct(uu);case"+":return ct(je(0));default:return Lr}}(t))}},r))}},f(Or,function(n){return f(yt,"",n)},f(yt,"\n",n))))),u=f(Ye,8,f(Se,function(n){return n+1},vu(f(Or,function(n){return n.a},Er(t))))),a=f(Ye,6,f(Se,function(n){return n+1},vu(f(Or,function(n){return n.b},Er(t)))));return{u:lt(f(ou,function(n){return 2===n.$},(r=t,o(kr,e(function(n,r,t){return f(Ar,r,t)}),h,r)))),j:o(Mt,E(1,1),au,t),o:!1,q:E(1,1),D:E(u,a)}},bu=function(n){return n.u?n.o?2:0:1},du={$:3},lu={$:4},$u={$:0},hu=t(function(n,r){return!f(oe,n,r).$}),gu=t(function(n,r){var t=r.j,e=r.D,u=r.q,a=r.u,i=f(xe,n,u),c=f(xe,n,i),v=f(oe,i,t),s=function(){if(v.$||2!==v.a.$)return a;return a-1}(),b=f(Ye,$u,f(Se,function(n){switch(function(n){switch(n.$){case 0:case 1:return 2;case 2:return 0;case 3:case 4:case 5:case 6:return 1;case 7:return 0;default:return 3}}(n)){case 0:return{$:2,a:n};case 1:return f(hu,c,t)?du:function(n){return{$:1,a:n}}(n);case 2:return du;default:return lu}},v));return{u:s,j:function(){switch(b.$){case 0:return o(ze,u,n,t);case 1:return o(ze,u,n,o(ze,i,n,t));case 2:return o(ze,u,n,t);case 3:default:return t}}(),o:4===b.$,q:function(){switch(b.$){case 0:case 1:case 2:return i;case 3:default:return u}}(),D:e}}),pu=Wn,mu=pu(h),wu=t(function(n,r){switch(n.$){case 0:return E(r,mu);case 1:if(bu(r.e))return E(r,mu);var t=r.s;return E(1===t.$?r:L(r,{e:f(gu,e=t.a,r.e)}),nu);case 2:var e=n.a;return bu(r.e)?E(r,mu):E(j(r.i,we)?L(r,{i:(u=e,{$:2,a:u})}):L(r,{i:we,e:f(gu,e,r.e)}),mu);case 3:return E(r,pu(m([nu,f(Wt,_t,qr(e=n.a))])));case 4:return E(f(Xe,Je(n.a),r),mu);case 5:return E(L(r,{x:n.a}),mu);case 6:return E(L(r,{e:su(r.x)}),mu);case 7:return E(L(r,{s:ct(e=n.a)}),f(Wt,_t,qr(e)));default:return E(L(r,{s:Lr}),mu)}var u}),yu=function(n){return{$:7,a:n}},Fu={$:8},ju=Cn,Au=t(function(n,r){return f(ju,n,{$:0,a:r})}),Wu={Z:!0,aa:!1},ku=t(function(n,r){return f(ju,n,{$:3,a:r})}),_u=cn,Eu=u(function(n,r,t,e){return{aK:r,a_:n,be:t,bi:e}}),Nu=U,Lu=u(function(n,r,t,e){return{aL:r,aW:n,a5:t,bb:e}}),Tu=Q,xu=o(Ot,t(function(n,r){return E(n,r)}),f(Nt,"clientX",Tu),f(Nt,"clientY",Tu)),qu=o(Ot,t(function(n,r){return E(n,r)}),f(Nt,"pageX",Tu),f(Nt,"pageY",Tu)),Cu=o(Ot,t(function(n,r){return E(n,r)}),f(Nt,"screenX",Tu),f(Nt,"screenY",Tu)),Bu=s(_u,Lu,f(Nt,"identifier",Nu),xu,qu,Cu),Iu=tn,Du=f(Rr,Ot(Ar),St(h)),Ru=function(n){var r=function(r){return f(Nt,mt(r),n)};return f(Iu,function(n){return Du(f(Or,r,f(gt,0,n-1)))},f(Nt,"length",Nu))},Ou=H,Su=s(_u,Eu,v(an,e(function(n,r,t){return{aF:n,aN:r,bc:t}}),f(Nt,"altKey",Ou),f(Nt,"ctrlKey",Ou),f(Nt,"shiftKey",Ou)),f(Nt,"changedTouches",Ru(Bu)),f(Nt,"targetTouches",Ru(Bu)),f(Nt,"touches",Ru(Bu))),Yu=e(function(n,r,t){return f(ku,n,f(Lt,function(n){return{n:t(n),Z:r.Z,aa:r.aa}},Su))}),zu=f(Yu,"touchcancel",Wu),Gu=f(Yu,"touchend",Wu),Pu=f(Yu,"touchstart",Wu),Ku=function(n){return m([(r=yu(n),f(Au,"mousedown",St(r))),function(n){return f(Au,"mouseup",St(n))}(Fu),Pu(function(){return yu(n)}),Gu(function(){return Fu}),zu(function(){return Fu})]);var r},Zu=qn("button"),Mu=qn("table"),Xu=qn("td"),Ju=Tn,Uu=qn("tr"),Hu=f(Mu,h,m([f(Uu,h,m([f(Xu,h,h),f(Xu,h,m([f(Zu,Ku(0),m([Ju("↑")]))])),f(Xu,h,h)])),f(Uu,h,m([f(Xu,h,m([f(Zu,Ku(2),m([Ju("←")]))])),f(Xu,h,m([f(Zu,Ku(1),m([Ju("↓")]))])),f(Xu,h,m([f(Zu,Ku(3),m([Ju("→")]))]))]))])),Qu=function(n){return"Stage { player: "+(t=(r=n.q).b,"("+mt(r.a)+", "+mt(t)+"), gems: "+mt(n.u)+"}");var r,t},Vu=function(n){return{$:5,a:n}},na=qn("div"),ra=qn("p"),ta=qn("textarea"),ea=hn,ua=t(function(n,r){return f(Bn,n,ea(r))}),aa=ua("value"),ia=function(n){return f(Au,"click",St(n))},ca=function(n){return E(n,!0)},fa=t(function(n,r){return f(ju,n,{$:1,a:r})}),oa=f(t(function(n,r){return o(Rr,Nt,r,n)}),m(["target","value"]),Tt),va=function(n){return f(na,h,m([f(ra,h,m([Ju("すて～じえでぃっと")])),f(ta,m([(t=Vu,f(fa,"input",f(Lt,ca,f(Lt,t,oa)))),aa(n),(r=12,f(In,"rows",mt(r))),function(n){return f(In,"cols",mt(n))}(12)]),h),f(Zu,m([ia(pr)]),m([Ju("よみこみ")]))]));var r,t},sa=t(function(n,r){if(r.b){var e=r.b;return f(Ar,r.a,o(Rr,t(function(r,t){return f(Ar,n,f(Ar,r,t))}),h,e))}return h}),ba=function(n){return f(wt,"",n)},da=xn("http://www.w3.org/2000/svg"),la=da("circle"),$a=da("g"),ha=da("polygon"),ga=da("polyline"),pa=da("rect"),ma=In("cx"),wa=In("cy"),ya=In("fill"),Fa=In("height"),ja=In("points"),Aa=In("r"),Wa=In("stroke"),ka=In("transform"),_a=In("width"),Ea=In("x"),Na=In("y"),La=function(n){switch(n.$){case 0:var r=n.b,t=n.c,e=n.d,u=n.e,a=n.f;return f(pa,m([Ea(mt(n.a)),Na(mt(r)),_a(mt(t)),Fa(mt(e)),Wa(u),ya(a)]),h);case 1:u=n.b,a=n.c;return f(ha,m([ja(ba(f(sa," ",f(Or,function(n){var r=n.b;return mt(n.a)+","+mt(r)},n.a)))),Wa(u),ya(a)]),h);case 2:var i=n.b;return f(ga,m([ja(ba(f(sa," ",f(Or,function(n){var r=n.b;return mt(n.a)+","+mt(r)},n.a)))),Wa(i),ya("none")]),h);case 3:r=n.b;var c=n.c;u=n.d,a=n.e;return f(la,m([ma(mt(n.a)),wa(mt(r)),Aa(mt(c)),Wa(u),ya(a)]),h);case 4:var o=n.b;return f($a,m([ka("rotate("+mt(n.a)+", 8, 8)")]),f(Or,La,o));default:o=n.a;return f($a,m([ka("scale(-1, 1) translate(-16, 0)")]),f(Or,La,o))}},Ta=a(function(n,r,t,e,u){return{$:3,a:n,b:r,c:t,d:e,e:u}}),xa=e(function(n,r,t){return{$:1,a:n,b:r,c:t}}),qa=t(function(n,r){return{$:2,a:n,b:r}}),Ca=i(function(n,r,t,e,u,a){return{$:0,a:n,b:r,c:t,d:e,e:u,f:a}}),Ba=t(function(n,r){return{$:4,a:n,b:r}}),Ia=function(n){switch(n.$){case 0:return m([s(Ta,8,8,6,"#000000","#00FF00")]);case 1:return m([b(Ca,0,0,16,16,"none","#AAAAAA"),b(Ca,4,4,8,8,"#FFFFFF","none")]);case 2:switch(n.a){case 0:return m([o(xa,m([E(0,8),E(8,8),E(8,0)]),"none","#8888FF"),o(xa,m([E(8,0),E(8,8),E(16,8)]),"none","#000088"),o(xa,m([E(16,8),E(8,8),E(8,16)]),"none","#0000FF"),o(xa,m([E(8,16),E(8,8),E(0,8)]),"none","#000088")]);case 1:return m([f(Ba,180,Ia(f(ye,0,0)))]);case 2:return m([f(Ba,270,Ia(f(ye,0,0)))]);default:return m([f(Ba,90,Ia(f(ye,0,0)))])}case 3:return m([b(Ca,1,1,14,14,"#000000","#FFFF00")]);case 4:switch(n.a){case 0:return m([b(Ca,1,1,14,14,"#000000","#FFFF00"),f(qa,m([E(8,13),E(8,3),E(3,8),E(13,8),E(8,3)]),"#FF0000")]);case 1:return m([f(Ba,180,Ia(Fe(0)))]);case 2:return m([f(Ba,270,Ia(Fe(0)))]);default:return m([f(Ba,90,Ia(Fe(0)))])}case 5:return m([b(Ca,1,1,14,14,"#000000","#FFFF00"),f(qa,m([E(11,11),E(5,11),E(5,5),E(11,5),E(8,8)]),"#FF0000")]);case 6:return m([(r=Ia(eu),{$:5,a:r})]);case 7:return m([b(Ca,1,1,14,14,"#888888","#FFFF88")]);default:return m([f(Ba,30*n.a,m([o(xa,m([E(5,1),E(11,1),E(11,3),E(9,3),E(9,7),E(13,7),E(13,5),E(15,5),E(15,11),E(13,11),E(13,9),E(9,9),E(9,13),E(11,13),E(11,15),E(5,15),E(5,13),E(7,13),E(7,9),E(3,9),E(3,11),E(1,11),E(1,5),E(3,5),E(3,7),E(7,7),E(7,3),E(5,3)]),"none","#000000"),s(Ta,8,8,3,"#000000","#FF00FF")]))])}var r},Da=e(function(n,r,t){return f($a,m([ka("translate("+mt(n)+", "+mt(r)+")")]),t)}),Ra=e(function(n,r,t){return o(Da,16*n,16*r,f(Or,La,Ia(t)))}),Oa=da("svg"),Sa=qn("input"),Ya=ua("type"),za=or({aY:function(){return E(Tr,f(Wt,function(){return pr},qr(0)))},bd:function(n){var r=n.i;if(2===r.$){var t=r.a;return f(me,50,function(){return{$:3,a:t}})}return ae(m([ue(xt),f(me,150,function(){return kt})]))},bj:wu,bl:function(n){var r;switch(bu(n.e)){case 0:return f(na,h,m([f(ra,h,m([Ju(function(n){var r=n.I,t=n.i;return"stage: "+Qu(n.e)+", frame: "+mt(r)+", inputState: "+function(n){switch(n.$){case 0:return"WaitForPalyerInput";case 1:return"WaitForEnemyTurn";default:return"ForceEnemyTurn"}}(t)}(n))])),(r=n.e,f(Oa,h,f(Or,function(n){var r=n.a;return o(Ra,r.a,r.b,n.b)},_r(r.j)))),Hu,va(n.x)]));case 1:return f(na,h,m([f(ra,h,m([Ju("くりあ～")])),f(Sa,m([Ya("button"),ia(pr),aa("リセット")]),h)]));default:return f(na,h,m([f(ra,h,m([Ju("ミス")])),f(Sa,m([Ya("button"),ia(pr),aa("リセット")]),h)]))}}});Qe={Paku2:{init:za(St(0))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?F(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Qe):n.Elm=Qe}(this);