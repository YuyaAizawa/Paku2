!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,function(r){return function(t){return n(r,t)}})}function e(n){return r(3,n,function(r){return function(t){return function(e){return n(r,t,e)}}})}function u(n){return r(4,n,function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}})}function i(n){return r(5,n,function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}})}function a(n){return r(6,n,function(r){return function(t){return function(e){return function(u){return function(i){return function(a){return n(r,t,e,u,i,a)}}}}}})}function f(n){return r(7,n,function(r){return function(t){return function(e){return function(u){return function(i){return function(a){return function(f){return n(r,t,e,u,i,a,f)}}}}}}})}function c(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function o(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function v(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function s(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function b(n,r,t,e,u,i,a){return 6===n.a?n.f(r,t,e,u,i,a):n(r)(t)(e)(u)(i)(a)}function d(n,r,t,e,u,i,a,f){return 7===n.a?n.f(r,t,e,u,i,a,f):n(r)(t)(e)(u)(i)(a)(f)}var l=e(function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e}),$=t(function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,E(t,r)}),h={$:0};function g(n,r){return{$:1,a:n,b:r}}var p=t(g);function F(n){for(var r=h,t=n.length;t--;)r=g(n[t],r);return r}function m(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var w=e(function(n,r,t){for(var e=[];r.b&&t.b;r=r.b,t=t.b)e.push(c(n,r.a,t.a));return F(e)});function y(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}function A(n,r){for(var t,e=[],u=j(n,r,0,e);u&&(t=e.pop());u=j(t.a,t.b,0,e));return u}function j(n,r,t,e){if(t>100)return e.push(E(n,r)),!0;if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&y(5),!1;for(var u in 0>n.$&&(n=mr(n),r=mr(r)),n)if(!j(n[u],r[u],t+1,e))return!1;return!0}function W(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=W(n.a,r.a))?t:(t=W(n.b,r.b))?t:W(n.c,r.c);for(;n.b&&r.b&&!(t=W(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var k=t(function(n,r){var t=W(n,r);return 0>t?hr:t?pr:$r}),_=0;function E(n,r){return{a:n,b:r}}function N(n,r,t){return{a:n,b:r,c:t}}function L(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function T(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=g(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=g(n.a,r);return t}function x(n){return{$:0,a:n}}function B(n){return{$:2,b:n,c:null}}var C=t(function(n,r){return{$:3,b:n,d:r}});var I=0;function S(n){var r={$:0,e:I++,f:n,g:null,h:[]};return D(r),r}function q(n){return B(function(r){r(x(S(n)))})}function O(n,r){n.h.push(r),D(n)}var G=t(function(n,r){return B(function(t){O(n,r),t(x(_))})});var R=!1,z=[];function D(n){if(z.push(n),!R){for(R=!0;n=z.shift();)P(n);R=!1}}function P(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b(function(r){n.f=r,D(n)}));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var M=Math.ceil,J=Math.floor,X=Math.log;var Y=t(function(n,r){return r.split(n)}),Q=t(function(n,r){return r.join(n)});function U(n){return{$:2,b:n}}U(function(n){return"number"!=typeof n?an("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?rt(n):!isFinite(n)||n%1?an("an INT",n):rt(n)}),U(function(n){return"boolean"==typeof n?rt(n):an("a BOOL",n)}),U(function(n){return"number"==typeof n?rt(n):an("a FLOAT",n)}),U(function(n){return rt(on(n))});var H=U(function(n){return"string"==typeof n?rt(n):n instanceof String?rt(n+""):an("a STRING",n)});var K=t(function(n,r){return{$:6,d:n,b:r}});function V(n,r){return{$:9,f:n,g:r}}var Z=t(function(n,r){return V(n,[r])}),nn=t(function(n,r){return rn(n,vn(r))});function rn(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?rt(n.c):an("null",r);case 3:return en(r)?tn(n.b,r,F):an("a LIST",r);case 4:return en(r)?tn(n.b,r,un):an("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return an("an OBJECT with a field named `"+t+"`",r);var e=rn(n.b,r[t]);return Ir(e)?e:nt(c(et,t,e.a));case 7:var u=n.e;if(!en(r))return an("an ARRAY",r);if(u>=r.length)return an("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=rn(n.b,r[u]);return Ir(e)?e:nt(c(ut,u,e.a));case 8:if("object"!=typeof r||null===r||en(r))return an("an OBJECT",r);var i=h;for(var a in r)if(r.hasOwnProperty(a)){e=rn(n.b,r[a]);if(!Ir(e))return nt(c(et,a,e.a));i=g(E(a,e.a),i)}return rt(_r(i));case 9:for(var f=n.f,o=n.g,v=0;o.length>v;v++){e=rn(o[v],r);if(!Ir(e))return e;f=f(e.a)}return rt(f);case 10:e=rn(n.b,r);return Ir(e)?rn(n.h(e.a),r):e;case 11:for(var s=h,b=n.g;b.b;b=b.b){e=rn(b.a,r);if(Ir(e))return e;s=g(e.a,s)}return nt(it(_r(s)));case 1:return nt(c(tt,n.a,on(r)));case 0:return rt(n.a)}}function tn(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var a=rn(n,r[i]);if(!Ir(a))return nt(c(ut,i,a.a));u[i]=a.a}return rt(t(u))}function en(n){return Array.isArray(n)||"function"==typeof FileList&&n instanceof FileList}function un(n){return c(Kr,n.length,function(r){return n[r]})}function an(n,r){return nt(c(tt,"Expecting "+n,on(r)))}function fn(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return fn(n.b,r.b);case 6:return n.d===r.d&&fn(n.b,r.b);case 7:return n.e===r.e&&fn(n.b,r.b);case 9:return n.f===r.f&&cn(n.g,r.g);case 10:return n.h===r.h&&fn(n.b,r.b);case 11:return cn(n.g,r.g)}}function cn(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!fn(n[e],r[e]))return!1;return!0}function on(n){return n}function vn(n){return n}on(null);function sn(n,r,t,e,u,i){var a=c(nn,n,on(r?r.flags:void 0));Ir(a)||y(2);var f={},o=(a=t(a.a)).a,v=i(b,o),s=function(n,r){var t;for(var e in bn){var u=bn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=ln(u,r)}return t}(f,b);function b(n,r){v(o=(a=c(e,n,o)).a,r),Fn(f,a.b,u(o))}return Fn(f,a.b,u(o)),s?{ports:s}:{}}var bn={};function dn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function ln(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,a=n.f;return t.h=S(c(C,function n(r){return c(C,n,{$:5,b:function(n){var f=n.a;return 0===n.$?o(u,t,f,r):i&&a?v(e,t,f.i,f.j,r):o(e,t,i?f.i:f.j,r)}})},n.b))}var $n=t(function(n,r){return B(function(t){n.g(r),t(x(_))})}),hn=t(function(n,r){return c(G,n.h,{$:0,a:r})});function gn(n){return function(r){return{$:1,k:n,l:r}}}function pn(n){return{$:2,m:n}}function Fn(n,r,t){var e={};for(var u in mn(!0,r,e,null),mn(!1,t,e,null),n)O(n[u],{$:"fx",a:e[u]||{i:h,j:h}})}function mn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){return c(n?bn[r].e:bn[r].f,function(n){for(var r=t;r;r=r.q)n=r.p(n);return n},e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:h,j:h},n?t.i=g(r,t.i):t.j=g(r,t.j),t}(n,i,t[u]));case 2:for(var a=r.m;a.b;a=a.b)mn(n,a.a,t,e);return;case 3:return void mn(n,r.o,t,{p:r.n,q:e})}}var wn;var yn="undefined"!=typeof document?document:{};function An(n,r){n.appendChild(r)}function jn(n){return{$:0,a:n}}var Wn=t(function(n,r){return t(function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b||0,u.push(a)}return i+=u.length,{$:1,c:r,d:Tn(t),e:u,f:n,b:i}})}),kn=Wn(void 0);t(function(n,r){return t(function(t,e){for(var u=[],i=0;e.b;e=e.b){var a=e.a;i+=a.b.b||0,u.push(a)}return i+=u.length,{$:2,c:r,d:Tn(t),e:u,f:n,b:i}})})(void 0);var _n=t(function(n,r){return{$:"a0",n:n,o:r}}),En=t(function(n,r){return{$:"a2",n:n,o:r}}),Nn=t(function(n,r){return{$:"a3",n:n,o:r}});var Ln;function Tn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var a=r[e]||(r[e]={});"a3"===e&&"class"===u?xn(a,u,i):a[u]=i}else"className"===u?xn(r,u,vn(i)):r[u]=vn(i)}return r}function xn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Bn(n,r){var t=n.$;if(5===t)return Bn(n.k||(n.k=n.m()),r);if(0===t)return yn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(a=Bn(e,i)).elm_event_node_ref=i,a}if(3===t)return Cn(a=n.h(n.g),r,n.d),a;var a=n.f?yn.createElementNS(n.f,n.c):yn.createElement(n.c);wn&&"a"==n.c&&a.addEventListener("click",wn(a)),Cn(a,r,n.d);for(var f=n.e,c=0;f.length>c;c++)An(a,Bn(1===t?f[c]:f[c].b,r));return a}function Cn(n,r,t){for(var e in t){var u=t[e];"a1"===e?In(n,u):"a0"===e?On(n,r,u):"a3"===e?Sn(n,u):"a4"===e?qn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function In(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Sn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function qn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function On(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],a=e[u];if(i){if(a){if(a.q.$===i.$){a.q=i;continue}n.removeEventListener(u,a)}a=Gn(r,i),n.addEventListener(u,a,Ln&&{passive:2>xt(i)}),e[u]=a}else n.removeEventListener(u,a),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Ln=!0}}))}catch(n){}function Gn(n,r){function t(r){var e=t.q,u=rn(e.a,r);if(Ir(u)){for(var i,a=xt(e),f=u.a,c=a?3>a?f.a:f.n:f,o=1==a?f.b:3==a&&f.S,v=(o&&r.stopPropagation(),(2==a?f.b:3==a&&f.Q)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)c=i(c);else for(var s=i.length;s--;)c=i[s](c);v=v.p}v(c,o)}}return t.q=r,t}function Rn(n,r){return n.$==r.$&&fn(n.a,r.a)}function zn(n,r){var t=[];return Pn(n,r,t,0),t}function Dn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Pn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Dn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var a=n.l,f=r.l,c=a.length,o=c===f.length;o&&c--;)o=a[c]===f[c];if(o)return void(r.k=n.k);r.k=r.m();var v=[];return Pn(n.k,r.k,v,0),void(v.length>0&&Dn(t,1,e,v));case 4:for(var s=n.j,b=r.j,d=!1,l=n.k;4===l.$;)d=!0,"object"!=typeof s?s=[s,l.j]:s.push(l.j),l=l.k;for(var $=r.k;4===$.$;)d=!0,"object"!=typeof b?b=[b,$.j]:b.push($.j),$=$.k;return d&&s.length!==b.length?void Dn(t,0,e,r):((d?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Dn(t,2,e,b),void Pn(l,$,t,e+1));case 0:return void(n.a!==r.a&&Dn(t,3,e,r.a));case 1:return void Mn(n,r,t,e,Xn);case 2:return void Mn(n,r,t,e,Yn);case 3:if(n.h!==r.h)return void Dn(t,0,e,r);var h=Jn(n.d,r.d);h&&Dn(t,4,e,h);var g=r.i(n.g,r.g);return void(g&&Dn(t,5,e,g))}}}function Mn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=Jn(n.d,r.d);i&&Dn(t,4,e,i),u(n,r,t,e)}else Dn(t,0,e,r)}function Jn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],a=r[u];i===a&&"value"!==u&&"checked"!==u||"a0"===t&&Rn(i,a)||((e=e||{})[u]=a)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=Jn(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var c in r)c in n||((e=e||{})[c]=r[c]);return e}function Xn(n,r,t,e){var u=n.e,i=r.e,a=u.length,f=i.length;a>f?Dn(t,6,e,{v:f,i:a-f}):f>a&&Dn(t,7,e,{v:a,e:i});for(var c=f>a?a:f,o=0;c>o;o++){var v=u[o];Pn(v,i[o],t,++e),e+=v.b||0}}function Yn(n,r,t,e){for(var u=[],i={},a=[],f=n.e,c=r.e,o=f.length,v=c.length,s=0,b=0,d=e;o>s&&v>b;){var l=(k=f[s]).a,$=(_=c[b]).a,h=k.b,g=_.b,p=void 0,F=void 0;if(l!==$){var m=f[s+1],w=c[b+1];if(m){var y=m.a,A=m.b;F=$===y}if(w){var j=w.a,W=w.b;p=l===j}if(p&&F)Pn(h,W,u,++d),Un(i,u,l,g,b,a),d+=h.b||0,Hn(i,u,l,A,++d),d+=A.b||0,s+=2,b+=2;else if(p)d++,Un(i,u,$,g,b,a),Pn(h,W,u,d),d+=h.b||0,s+=1,b+=2;else if(F)Hn(i,u,l,h,++d),d+=h.b||0,Pn(A,g,u,++d),d+=A.b||0,s+=2,b+=1;else{if(!m||y!==j)break;Hn(i,u,l,h,++d),Un(i,u,$,g,b,a),d+=h.b||0,Pn(A,W,u,++d),d+=A.b||0,s+=2,b+=2}}else Pn(h,g,u,++d),d+=h.b||0,s++,b++}for(;o>s;){var k;Hn(i,u,(k=f[s]).a,h=k.b,++d),d+=h.b||0,s++}for(;v>b;){var _,E=E||[];Un(i,u,(_=c[b]).a,_.b,void 0,E),b++}(u.length>0||a.length>0||E)&&Dn(t,8,e,{w:u,x:a,y:E})}var Qn="_elmW6BL";function Un(n,r,t,e,u,i){var a=n[t];if(!a)return i.push({r:u,A:a={c:0,z:e,r:u,s:void 0}}),void(n[t]=a);if(1===a.c){i.push({r:u,A:a}),a.c=2;var f=[];return Pn(a.z,e,f,a.r),a.r=u,void(a.s.s={w:f,A:a})}Un(n,r,t+Qn,e,u,i)}function Hn(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var a=[];return Pn(e,i.z,a,u),void Dn(r,9,u,{w:a,A:i})}Hn(n,r,t+Qn,e,u)}else{var f=Dn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Kn(n,r,t,e){!function n(r,t,e,u,i,a,f){var c=e[u];var o=c.r;for(;o===i;){var v=c.$;if(1===v)Kn(r,t.k,c.s,f);else if(8===v){c.t=r,c.u=f;var s=c.s.w;s.length>0&&n(r,t,s,0,i,a,f)}else if(9===v){c.t=r,c.u=f;var b=c.s;if(b){b.A.s=r;var s=b.w;s.length>0&&n(r,t,s,0,i,a,f)}}else c.t=r,c.u=f;if(!(c=e[++u])||(o=c.r)>a)return u}var d=t.$;if(4===d){for(var l=t.k;4===l.$;)l=l.k;return n(r,l,e,u,i+1,a,r.elm_event_node_ref)}var $=t.e;var h=r.childNodes;for(var g=0;$.length>g;g++){var p=1===d?$[g]:$[g].b,F=++i+(p.b||0);if(o>=i&&F>=o&&(u=n(h[g],p,e,u,i,F,f),!(c=e[u])||(o=c.r)>a))return u;i=F}return u}(n,r,t,0,0,r.b,e)}function Vn(n,r,t,e){return 0===t.length?n:(Kn(n,r,t,e),Zn(n,t))}function Zn(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=nr(u,e);u===n&&(n=i)}return n}function nr(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Bn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Cn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Zn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Bn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var a=t.A;return void 0!==a.r&&n.parentNode.removeChild(n),a.s=Zn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=yn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e],i=u.A;An(t,2===i.c?i.s:Bn(i.z,r.u))}return t}(t.y,r);n=Zn(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var a=u[i],f=a.A,c=2===f.c?f.s:Bn(f.z,r.u);n.insertBefore(c,n.childNodes[a.r])}e&&An(n,e);return n}(n,r);case 5:return r.s(n);default:y(10)}}function rr(n){if(3===n.nodeType)return jn(n.textContent);if(1!==n.nodeType)return jn("");for(var r=h,t=n.attributes,e=t.length;e--;){var u=t[e];r=g(c(Nn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),a=h,f=n.childNodes;for(e=f.length;e--;)a=g(rr(f[e]),a);return o(kn,i,r,a)}var tr=u(function(n,r,t,e){return sn(r,e,n.ay,n.aG,n.aE,function(r,t){var u=n.aI,i=e.node,a=rr(i);return ur(t,function(n){var t=u(n),e=zn(a,t);i=Vn(i,a,e,r),a=t})})}),er=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function ur(n,r){r(n);var t=0;function e(){t=1===t?0:(er(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&er(e),t=2)}}var ir={addEventListener:function(){},removeEventListener:function(){}},ar="undefined"!=typeof document?document:ir,fr="undefined"!=typeof window?window:ir,cr=e(function(n,r,t){return q(B(function(){function e(n){S(t(n))}return n.addEventListener(r,e,Ln&&{passive:!0}),function(){n.removeEventListener(r,e)}}))}),or=t(function(n,r){var t=rn(n,r);return Ir(t)?Vr(t.a):Zr});var vr=t(function(n,r){return B(function(){var t=setInterval(function(){S(r)},n);return function(){clearInterval(t)}})});var sr={$:5},br={$:0},dr={$:-2},lr=dr,$r=1,hr=0,gr=p,pr=2,Fr=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=o(n,t.b,t.c,o(Fr,n,r,t.e));n=u,r=i,t=e}}),mr=function(n){return o(Fr,e(function(n,r,t){return c(gr,E(n,r),t)}),h,n)},wr={r:0,j:lr,u:E(0,0)},yr={F:0,i:br,h:wr,v:"WWWWWWWWW\nW   ,   W\nW  ,  2,W\nW    ;  W\nWG,     W\nW B   ,GW\nWWWWWWWWW"},Ar=function(n){return n},jr=x,Wr=jr(0),kr=e(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=c(n,t.a,r);n=u,r=i,t=e}}),_r=function(n){return o(kr,gr,h,n)},Er=u(function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var a=i.a,f=i.b;if(f.b){var s=f.a,b=f.b;if(b.b){var d=b.b;return c(n,u,c(n,a,c(n,s,c(n,b.a,t>500?o(kr,n,r,_r(d)):v(Er,n,r,t+1,d)))))}return c(n,u,c(n,a,c(n,s,r)))}return c(n,u,c(n,a,r))}return c(n,u,r)}return r}),Nr=e(function(n,r,t){return v(Er,n,r,0,t)}),Lr=t(function(n,r){return o(Nr,t(function(r,t){return c(gr,n(r),t)}),h,r)}),Tr=C,xr=t(function(n,r){return c(Tr,function(r){return jr(n(r))},r)}),Br=e(function(n,r,t){return c(Tr,function(r){return c(Tr,function(t){return jr(c(n,r,t))},t)},r)}),Cr=function(n){return o(Nr,Br(gr),jr(h),n)},Ir=function(n){return!n.$},Sr=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),qr=M,Or=t(function(n,r){return X(r)/X(n)}),Gr=qr(c(Or,2,32)),Rr=[],zr=v(Sr,0,Gr,Rr,Rr),Dr=$,Pr=t(function(n,r){for(;;){var t=c(Dr,32,n),e=t.b,u=c(gr,{$:0,a:t.a},r);if(!e.b)return _r(u);n=e,r=u}}),Mr=t(function(n,r){for(;;){var t=qr(r/32);if(1===t)return c(Dr,32,n).a;n=c(Pr,n,h),r=t}}),Jr=J,Xr=t(function(n,r){return W(n,r)>0?n:r}),Yr=function(n){return n.length},Qr=t(function(n,r){if(r.a){var t=32*r.a,e=Jr(c(Or,32,t-1)),u=n?_r(r.d):r.d,i=c(Mr,u,r.a);return v(Sr,Yr(r.c)+t,c(Xr,5,e*Gr),i,r.c)}return v(Sr,Yr(r.c),Gr,Rr,r.c)}),Ur=l,Hr=i(function(n,r,t,e,u){for(;;){if(0>r)return c(Qr,!1,{d:e,a:t/32|0,c:u});var i={$:1,a:o(Ur,32,r,n)};n=n,r=r-32,t=t,e=c(gr,i,e),u=u}}),Kr=t(function(n,r){if(n>0){var t=n%32;return s(Hr,r,n-t-32,n,h,o(Ur,t,n-t,r))}return zr}),Vr=function(n){return{$:0,a:n}},Zr={$:1},nt=function(n){return{$:1,a:n}},rt=function(n){return{$:0,a:n}},tt=t(function(n,r){return{$:3,a:n,b:r}}),et=t(function(n,r){return{$:0,a:n,b:r}}),ut=t(function(n,r){return{$:1,a:n,b:r}}),it=function(n){return{$:2,a:n}},at=function(n){return o(kr,t(function(n,r){return r+1}),0,n)},ft=w,ct=e(function(n,r,t){for(;;){if(W(n,r)>=1)return t;var e=n,u=r-1,i=c(gr,r,t);n=e,r=u,t=i}}),ot=t(function(n,r){return o(ct,n,r,h)}),vt=t(function(n,r){return o(ft,n,c(ot,0,at(r)-1),r)}),st=function(n){return n+""},bt=t(function(n,r){return c(Q,n,m(r))}),dt=t(function(n,r){return F(c(Y,n,r))}),lt=$n,$t=t(function(n,r){var t=r;return q(c(Tr,lt(n),t))});bn.Task=dn(Wr,e(function(n,r){return c(xr,function(){return 0},Cr(c(Lr,$t(n),r)))}),e(function(){return jr(0)}),t(function(n,r){return c(xr,n,r)}));var ht=gn("Task"),gt=t(function(n,r){return ht(c(xr,n,r))}),pt={$:1},Ft=function(n){return{$:2,a:n}},mt={$:0},wt=K,yt=Z,At=H,jt=c(yt,function(n){switch(n){case"ArrowUp":return Ft(0);case"ArrowDown":return Ft(1);case"ArrowRight":return Ft(3);case"ArrowLeft":return Ft(2);default:return mt}},c(wt,"key",At)),Wt=e(function(n,r,t){return{$:0,a:n,b:r,c:t}}),kt=t(function(n,r){return{ad:r,am:n}}),_t=jr(c(kt,h,lr)),Et=function(n){var r=n.b;return E(T(function(n){return n?"w_":"d_"}(n.a),r),n)},Nt=t(function(n,r){return{X:r,_:n}}),Lt=hn,Tt=function(n){return{$:0,a:n}},xt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Bt=e(function(n,r,t){return c(xr,function(n){return E(r,n)},o(cr,t.a?fr:ar,t.b,function(t){return c(Lt,n,c(Nt,r,t))}))}),Ct=i(function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}}),It=k,St=i(function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return s(Ct,n,r,t,e,u);var i=e.d;v=e.e;return s(Ct,0,e.b,e.c,s(Ct,1,i.b,i.c,i.d,i.e),s(Ct,1,r,t,v,u))}var a=u.b,f=u.c,c=u.d,o=u.e;if(-1!==e.$||e.a)return s(Ct,n,a,f,s(Ct,0,r,t,e,c),o);var v;return s(Ct,0,r,t,s(Ct,1,e.b,e.c,e.d,v=e.e),s(Ct,1,a,f,c,o))}),qt=e(function(n,r,t){if(-2===t.$)return s(Ct,0,n,r,dr,dr);var e=t.a,u=t.b,i=t.c,a=t.d,f=t.e;switch(c(It,n,u)){case 0:return s(St,e,u,i,o(qt,n,r,a),f);case 1:return s(Ct,e,u,r,a,f);default:return s(St,e,u,i,a,o(qt,n,r,f))}}),Ot=e(function(n,r,t){var e=o(qt,n,r,t);if(-1!==e.$||e.a)return e;return s(Ct,1,e.b,e.c,e.d,e.e)}),Gt=function(n){return o(kr,t(function(n,r){return o(Ot,n.a,n.b,r)}),lr,n)},Rt=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=o(n,t.b,t.c,o(Rt,n,r,t.d));n=u,r=i,t=e}}),zt=a(function(n,r,u,i,a,f){var c=o(Rt,e(function(t,e,i){n:for(;;){var a=i.a,f=i.b;if(a.b){var c=a.a,s=c.a,b=c.b,d=a.b;if(0>W(s,t)){t=t,e=e,i=E(d,o(n,s,b,f));continue n}return W(s,t)>0?E(a,o(u,t,e,f)):E(d,v(r,s,b,e,f))}return E(a,o(u,t,e,f))}}),E(mr(i),f),a),s=c.a,b=c.b;return o(kr,t(function(r,t){return o(n,r.a,r.b,t)}),b,s)}),Dt=t(function(n,r){return o(Rt,Ot,r,n)}),Pt=function(n){return B(function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(x(_))})},Mt=e(function(n,r,t){var i=e(function(r,t,e){var u=e.c;return N(e.a,e.b,c(gr,o(Bt,n,r,t),u))}),a=e(function(n,r,t){var e=t.b,u=t.c;return N(c(gr,r,t.a),e,u)}),f=u(function(n,r,t,e){var u=e.c;return N(e.a,o(Ot,n,r,e.b),u)}),v=c(Lr,Et,r),s=b(zt,a,f,i,t.ad,Gt(v),N(h,lr,h)),d=s.b,l=s.c;return c(Tr,function(n){return jr(c(kt,v,c(Dt,d,Gt(n))))},c(Tr,function(){return Cr(l)},Cr(c(Lr,Pt,s.a))))}),Jt=e(function(n,r,t){var e=n(r);return e.$?t:c(gr,e.a,t)}),Xt=t(function(n,r){return o(Nr,Jt(n),h,r)});bn["Browser.Events"]=dn(_t,Mt,e(function(n,r,t){var e=r._,u=r.X,i=c(Xt,function(n){var r=n.b,t=r.c;return A(n.a,e)?c(or,t,u):Zr},t.am);return c(Tr,function(){return jr(t)},Cr(c(Lr,lt(n),i)))}),0,t(function(n,r){return o(Wt,r.a,r.b,c(yt,n,r.c))}));var Yt,Qt=gn("Browser.Events"),Ut=c(e(function(n,r,t){return Qt(o(Wt,n,r,t))}),0,"keydown"),Ht=pn,Kt=t(function(n,r){return{$:0,a:n,b:r}}),Vt=t(function(n,r){return{ag:r,an:n}}),Zt=jr(c(Vt,lr,lr)),ne=t(function(n,r){n:for(;;){if(-2===r.$)return Zr;var t=r.c,e=r.d,u=r.e;switch(c(It,n,r.b)){case 0:n=n,r=e;continue n;case 1:return Vr(t);default:n=n,r=u;continue n}}}),re=t(function(n,r){var t=n.a,e=n.b,u=c(ne,t,r);return o(Ot,t,1===u.$?F([e]):c(gr,e,u.a),r)}),te=q,ee=vr,ue=e(function(n,r,t){if(r.b){var e=r.a,u=r.b,i=te(c(ee,e,c(Lt,n,e)));return c(Tr,function(r){return o(ue,n,u,o(Ot,e,r,t))},i)}return jr(t)}),ie=e(function(n,r,t){var i=t.ag,a=e(function(n,r,t){var e=t.c;return N(t.a,t.b,c(Tr,function(){return e},Pt(r)))}),f=o(kr,re,lr,r),v=b(zt,e(function(n,r,t){var e=t.b,u=t.c;return N(c(gr,n,t.a),e,u)}),u(function(n,r,t,e){var u=e.c;return N(e.a,o(Ot,n,t,e.b),u)}),a,f,i,N(h,lr,jr(0))),s=v.a,d=v.b;return c(Tr,function(n){return jr(c(Vt,f,n))},c(Tr,function(){return o(ue,n,s,d)},v.c))}),ae=(Yt=Ar,B(function(n){n(x(Yt(Date.now())))})),fe=e(function(n,r,t){var e=c(ne,r,t.an);if(1===e.$)return jr(t);var u=e.a;return c(Tr,function(){return jr(t)},c(Tr,function(r){return Cr(c(Lr,function(t){return c(lt,n,t(r))},u))},ae))}),ce=e(function(n,r,t){return n(r(t))});bn.Time=dn(Zt,ie,fe,0,t(function(n,r){return c(Kt,r.a,c(ce,n,r.b))}));var oe,ve=gn("Time"),se=t(function(n,r){return ve(c(Kt,n,r))}),be={$:1},de=function(n){return{$:4,a:n}},le=t(function(n,r){var t=r.a,e=r.b;switch(n){case 0:return E(t,e-1);case 1:return E(t,e+1);case 2:return E(t-1,e);default:return E(t+1,e)}}),$e=function(n){if(-1===n.$&&-1===n.d.$&&-1===n.e.$){if(-1!==n.e.d.$||n.e.d.a){var r=n.d,t=n.e;a=t.b,f=t.c,e=t.d,v=t.e;return s(Ct,1,n.b,n.c,s(Ct,0,r.b,r.c,r.d,r.e),s(Ct,0,a,f,e,v))}var e,u=n.d,i=n.e,a=i.b,f=i.c,c=(e=i.d,e.d),o=e.e,v=i.e;return s(Ct,0,e.b,e.c,s(Ct,1,n.b,n.c,s(Ct,0,u.b,u.c,u.d,u.e),c),s(Ct,1,a,f,o,v))}return n},he=function(n){if(-1===n.$&&-1===n.d.$&&-1===n.e.$){if(-1!==n.d.d.$||n.d.d.a){var r=n.d,t=r.d,e=n.e;v=e.b,b=e.c,d=e.d,l=e.e;return s(Ct,1,u=n.b,i=n.c,s(Ct,0,r.b,r.c,t,c=r.e),s(Ct,0,v,b,d,l))}var u=n.b,i=n.c,a=n.d,f=a.d,c=a.e,o=n.e,v=o.b,b=o.c,d=o.d,l=o.e;return s(Ct,0,a.b,a.c,s(Ct,1,f.b,f.c,f.d,f.e),s(Ct,1,u,i,c,s(Ct,0,v,b,d,l)))}return n},ge=f(function(n,r,t,e,u,i,a){if(-1!==i.$||i.a){n:for(;;){if(-1===a.$&&1===a.a){if(-1===a.d.$){if(1===a.d.a){return he(r)}break n}return he(r)}break n}return r}return s(Ct,t,i.b,i.c,i.d,s(Ct,0,e,u,i.e,a))}),pe=function(n){if(-1===n.$&&-1===n.d.$){var r=n.a,t=n.b,e=n.c,u=n.d,i=u.d,a=n.e;if(1===u.a){if(-1!==i.$||i.a){var f=$e(n);if(-1===f.$){var c=f.e;return s(St,f.a,f.b,f.c,pe(f.d),c)}return dr}return s(Ct,r,t,e,pe(u),a)}return s(Ct,r,t,e,pe(u),a)}return dr},Fe=t(function(n,r){if(-2===r.$)return dr;var t=r.a,e=r.b,u=r.c,i=r.d,a=r.e;if(0>W(n,e)){if(-1===i.$&&1===i.a){var f=i.d;if(-1!==f.$||f.a){var o=$e(r);if(-1===o.$){var v=o.e;return s(St,o.a,o.b,o.c,c(Fe,n,o.d),v)}return dr}return s(Ct,t,e,u,c(Fe,n,i),a)}return s(Ct,t,e,u,c(Fe,n,i),a)}return c(me,n,d(ge,n,r,t,e,u,i,a))}),me=t(function(n,r){if(-1===r.$){var t=r.a,e=r.b,u=r.c,i=r.d,a=r.e;if(A(n,e)){var f=function(n){for(;;){if(-1!==n.$||-1!==n.d.$)return n;n=n.d}}(a);return-1===f.$?s(St,t,f.b,f.c,i,pe(a)):dr}return s(St,t,e,u,i,c(Fe,n,a))}return dr}),we=t(function(n,r){var t=c(Fe,n,r);if(-1!==t.$||t.a)return t;return s(Ct,1,t.b,t.c,t.d,t.e)}),ye=t(function(n,r){return r.$?Zr:Vr(n(r.a))}),Ae=t(function(n,r){return r.$?n:r.a}),je=e(function(n,r,t){var e=c(le,r,n);return c(Ae,t,c(ye,function(r){return o(Ot,e,r,c(we,n,t))},c(ne,n,t)))}),We=function(n){var r=n.v;return{F:n.F+1,i:br,h:function(n){var r=e(function(n,r,t){if(4!==r.$)return t;var e=r.a,u=c(ne,c(le,e,n),t);if(1===u.$)return o(je,n,e,t);switch(u.a.$){case 5:return o(Ot,n,de(function(n){switch(n){case 0:return 3;case 1:return 2;case 2:return 0;default:return 1}}(e)),t);case 6:return o(Ot,n,de(function(n){switch(n){case 0:return 2;case 1:return 3;case 2:return 1;default:return 0}}(e)),t);default:return t}});return L(n,{j:o(Nr,function(n){return c(r,n.a,n.b)},n.j,mr(n.j))})}(n.h),v:r}},ke={$:6},_e={$:3},Ee={$:5},Ne={$:2},Le={$:0},Te={$:1},xe=t(function(n,r){return r.b?o(Nr,gr,r,n):n}),Be=t(function(n,r){return o(Nr,xe,h,c(Lr,n,r))}),Ce=t(function(n,r){return o(Nr,t(function(r,t){return n(r)?c(gr,r,t):t}),h,r)}),Ie=function(n){var r,t=Gt(c(Be,function(n){return n},c(vt,function(n){return function(r){return c(Xt,function(n){return n},c(vt,function(r){return function(t){return c(ye,function(t){return E(E(r,n),t)},function(n){switch(n){case"W":return Vr(Te);case"G":return Vr(Ne);case"B":return Vr(_e);case"8":return Vr(de(0));case"2":return Vr(de(1));case"4":return Vr(de(2));case"6":return Vr(de(3));case",":return Vr(Ee);case";":return Vr(ke);default:return Zr}}(t))}},r))}},c(Lr,function(n){return c(dt,"",n)},c(dt,"\n",n)))));return{r:at(c(Ce,function(n){return A(n,Ne)},(r=t,o(Fr,e(function(n,r,t){return c(gr,r,t)}),h,r)))),j:o(Ot,E(1,1),Le,t),u:E(1,1)}},Se={$:3},qe={$:0},Oe=t(function(n,r){return!c(ne,n,r).$}),Ge=t(function(n,r){var t=r.j,e=r.u,u=r.r,i=c(le,n,e),a=c(le,n,i),f=c(ne,i,t),v=A(f,Vr(Ne))?u-1:u,s=c(Ae,qe,c(ye,function(n){switch(function(n){switch(n.$){case 0:case 1:return 2;case 2:return 0;case 3:case 4:case 5:default:return 1}}(n)){case 0:return{$:2,a:n};case 1:return c(Oe,a,t)?Se:function(n){return{$:1,a:n}}(n);default:return Se}},f));return{r:v,j:function(){switch(s.$){case 0:return o(je,e,n,t);case 1:return o(je,e,n,o(je,i,n,t));case 2:return o(je,e,n,t);default:return t}}(),u:function(){switch(s.$){case 0:case 1:case 2:return i;default:return e}}()}}),Re=pn(h),ze=t(function(n,r){switch(n.$){case 0:return E(r,Re);case 1:return E(We(r),Re);case 2:var t=n.a;return E(A(r.i,be)?L(r,{i:(e=t,{$:2,a:e})}):L(r,{i:be,h:c(Ge,t,r.h)}),Re);case 3:t=n.a;return E(We(r),c(gt,Ft,jr(t)));case 4:return E(L(r,{v:n.a}),Re);default:return E(L(r,{h:Ie(r.v)}),Re)}var e}),De=kn("button"),Pe=kn("table"),Me=kn("td"),Je=jn,Xe=kn("tr"),Ye=_n,Qe=t(function(n,r){return c(Ye,n,{$:0,a:r})}),Ue=function(n){return c(Qe,"click",Tt(n))},He=c(Pe,h,F([c(Xe,h,F([c(Me,h,h),c(Me,h,F([c(De,F([Ue(Ft(0))]),F([Je("↑")]))])),c(Me,h,h)])),c(Xe,h,F([c(Me,h,F([c(De,F([Ue(Ft(2))]),F([Je("←")]))])),c(Me,h,F([c(De,F([Ue(Ft(1))]),F([Je("↓")]))])),c(Me,h,F([c(De,F([Ue(Ft(3))]),F([Je("→")]))]))]))])),Ke=function(n){return"Stage { player: "+(t=(r=n.u).b,"("+st(r.a)+", "+st(t)+"), gems: "+st(n.r)+"}");var r,t},Ve=function(n){return{$:4,a:n}},Ze=kn("div"),nu=kn("p"),ru=kn("textarea"),tu=on,eu=t(function(n,r){return c(En,n,tu(r))}),uu=eu("value"),iu=function(n){return E(n,!0)},au=t(function(n,r){return c(Ye,n,{$:1,a:r})}),fu=c(t(function(n,r){return o(Nr,wt,r,n)}),F(["target","value"]),At),cu=function(n){return c(Ze,h,F([c(nu,h,F([Je("すて～じえでぃっと")])),c(ru,F([(t=Ve,c(au,"input",c(yt,iu,c(yt,t,fu)))),uu(n),(r=12,c(Nn,"rows",st(r))),function(n){return c(Nn,"cols",st(n))}(12)]),h),c(De,F([Ue(sr)]),F([Je("よみこみ")]))]));var r,t},ou=t(function(n,r){if(r.b){var e=r.b;return c(gr,r.a,o(Nr,t(function(r,t){return c(gr,n,c(gr,r,t))}),h,e))}return h}),vu=function(n){return c(bt,"",n)},su=Wn("http://www.w3.org/2000/svg"),bu=su("circle"),du=su("polygon"),lu=su("polyline"),$u=su("rect"),hu=Nn("cx"),gu=Nn("cy"),pu=Nn("fill"),Fu=Nn("height"),mu=Nn("points"),wu=Nn("r"),yu=Nn("stroke"),Au=Nn("width"),ju=Nn("x"),Wu=Nn("y"),ku=e(function(n,r,t){switch(t.$){case 0:var e=t.b,u=t.c,i=t.d,a=t.e,f=t.f;return c($u,F([ju(st(n+t.a)),Wu(st(r+e)),Au(st(u)),Fu(st(i)),yu(a),pu(f)]),h);case 1:a=t.b,f=t.c;return c(du,F([mu(vu(c(ou," ",c(Lr,function(t){var e=t.b;return st(n+t.a)+","+st(r+e)},t.a)))),yu(a),pu(f)]),h);case 2:var o=t.b;return c(lu,F([mu(vu(c(ou," ",c(Lr,function(t){var e=t.b;return st(n+t.a)+","+st(r+e)},t.a)))),yu(o),pu("none")]),h);default:e=t.b;var v=t.c;a=t.d,f=t.e;return c(bu,F([hu(st(n+t.a)),gu(st(r+e)),wu(st(v)),yu(a),pu(f)]),h)}}),_u=i(function(n,r,t,e,u){return{$:3,a:n,b:r,c:t,d:e,e:u}}),Eu=e(function(n,r,t){return{$:1,a:n,b:r,c:t}}),Nu=t(function(n,r){return{$:2,a:n,b:r}}),Lu=a(function(n,r,t,e,u,i){return{$:0,a:n,b:r,c:t,d:e,e:u,f:i}}),Tu=e(function(n,r,t){return c(Lr,c(ku,16*n,16*r),function(n){switch(n.$){case 0:return F([s(_u,8,8,6,"#000000","#00FF00")]);case 1:return F([b(Lu,0,0,16,16,"none","#AAAAAA"),b(Lu,4,4,8,8,"#FFFFFF","none")]);case 2:return F([o(Eu,F([E(0,8),E(8,8),E(8,0)]),"none","#0000FF"),o(Eu,F([E(8,0),E(8,8),E(16,8)]),"none","#000088"),o(Eu,F([E(16,8),E(8,8),E(8,16)]),"none","#0000FF"),o(Eu,F([E(8,16),E(8,8),E(0,8)]),"none","#000088")]);case 3:return F([b(Lu,1,1,14,14,"#000000","#FFFF00")]);case 4:switch(n.a){case 0:return F([b(Lu,1,1,14,14,"#000000","#FFFF00"),c(Nu,F([E(8,13),E(8,3),E(3,8),E(13,8),E(8,3)]),"#FF0000")]);case 1:return F([b(Lu,1,1,14,14,"#000000","#FFFF00"),c(Nu,F([E(8,3),E(8,13),E(3,8),E(13,8),E(8,13)]),"#FF0000")]);case 2:return F([b(Lu,1,1,14,14,"#000000","#FFFF00"),c(Nu,F([E(13,8),E(3,8),E(8,3),E(8,13),E(3,8)]),"#FF0000")]);default:return F([b(Lu,1,1,14,14,"#000000","#FFFF00"),c(Nu,F([E(3,8),E(13,8),E(8,3),E(8,13),E(13,8)]),"#FF0000")])}case 5:return F([b(Lu,1,1,14,14,"#000000","#FFFF00"),c(Nu,F([E(11,11),E(5,11),E(5,5),E(11,5),E(8,8)]),"#FF0000")]);default:return F([b(Lu,1,1,14,14,"#000000","#FFFF00"),c(Nu,F([E(5,11),E(11,11),E(11,5),E(5,5),E(8,8)]),"#FF0000")])}}(t))}),xu=su("svg"),Bu=kn("input"),Cu=eu("type"),Iu=tr({ay:function(){return E(yr,c(gt,function(){return sr},jr(0)))},aE:function(n){var r=n.i;if(2===r.$){var t=r.a;return c(se,50,function(){return{$:3,a:t}})}return Ht(F([Ut(jt),c(se,150,function(){return pt})]))},aG:ze,aI:function(n){return c(Ze,h,F(n.h.r?[c(nu,h,F([Je((r=n,t=r.F,e=r.i,"stage: "+Ke(r.h)+", frame: "+st(t)+", inputState: "+function(n){switch(n.$){case 0:return"WaitForPalyerInput";case 1:return"WaitForEnemyTurn";default:return"ForceEnemyTurn"}}(e)))])),function(n){return c(xu,h,c(Be,function(n){var r=n.a;return o(Tu,r.a,r.b,n.b)},mr(n.j)))}(n.h),He,cu(n.v)]:[c(nu,h,F([Je("くりあ～")])),c(Bu,F([Cu("button"),Ue(sr),uu("リセット")]),h)]));var r,t,e}});oe={Paku2:{init:Iu(Tt(0))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?y(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,oe):n.Elm=oe}(this);