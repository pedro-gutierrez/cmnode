/**
 * @license
 * Copyright 2015 The Incremental DOM Authors. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS-IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
!function(t,e){"object"==typeof exports&&"undefined"!=typeof module?e(exports):"function"==typeof define&&define.amd?define(["exports"],e):e(t.IncrementalDOM={})}(this,function(t){"use strict";function e(){}function n(t,e){this.attrs=a(),this.attrsArr=[],this.newAttrs=a(),this.staticsApplied=!1,this.key=e,this.keyMap=a(),this.keyMapValid=!0,this.focused=!1,this.nodeName=t,this.text=null}function r(){this.created=h.nodesCreated&&[],this.deleted=h.nodesDeleted&&[]}var i=Object.prototype.hasOwnProperty;e.prototype=Object.create(null);var o=function(t,e){return i.call(t,e)},a=function(){return new e},u="__incrementalDOMData",l=function(t,e,r){var i=new n(e,r);return t[u]=i,i},f=function(t){return c(t),t[u]},c=function(t){if(!t[u]){var e=t instanceof Element,n=e?t.localName:t.nodeName,r=e?t.getAttribute("key"):null,i=l(t,n,r);if(r&&(f(t.parentNode).keyMap[r]=t),e)for(var o=t.attributes,a=i.attrs,s=i.newAttrs,d=i.attrsArr,p=0;p<o.length;p+=1){var h=o[p],v=h.name,m=h.value;a[v]=m,s[v]=void 0,d.push(v),d.push(m)}for(var y=t.firstChild;y;y=y.nextSibling)c(y)}},s=function(t,e){return"svg"===t?"http://www.w3.org/2000/svg":"foreignObject"===f(e).nodeName?null:e.namespaceURI},d=function(t,e,n,r){var i=s(n,e),o=void 0;return o=i?t.createElementNS(i,n):t.createElement(n),l(o,n,r),o},p=function(t){var e=t.createTextNode("");return l(e,"#text",null),e},h={nodesCreated:null,nodesDeleted:null};r.prototype.markCreated=function(t){this.created&&this.created.push(t)},r.prototype.markDeleted=function(t){this.deleted&&this.deleted.push(t)},r.prototype.notifyChanges=function(){this.created&&this.created.length>0&&h.nodesCreated(this.created),this.deleted&&this.deleted.length>0&&h.nodesDeleted(this.deleted)};var v=function(t){return t instanceof Document||t instanceof DocumentFragment},m=function(t,e){for(var n=[],r=t;r!==e;)n.push(r),r=r.parentNode;return n},y=function(t){for(var e=t,n=e;e;)n=e,e=e.parentNode;return n},g=function(t){var e=y(t);return v(e)?e.activeElement:null},k=function(t,e){var n=g(t);return n&&t.contains(n)?m(n,e):[]},x=function(t,e,n){for(var r=e.nextSibling,i=n;i!==e;){var o=i.nextSibling;t.insertBefore(i,r),i=o}},w=null,b=null,N=null,C=null,A=function(t,e){for(var n=0;n<t.length;n+=1)f(t[n]).focused=e},D=function(t){var e=function(e,n,i){var o=w,a=C,u=b,l=N;w=new r,C=e.ownerDocument,N=e.parentNode;var f=k(e,N);A(f,!0);var c=t(e,n,i);return A(f,!1),w.notifyChanges(),w=o,C=a,b=u,N=l,c};return e},O=D(function(t,e,n){return b=t,V(),e(n),T(),t}),M=D(function(t,e,n){var r={nextSibling:t};return b=r,e(n),t!==b&&t.parentNode&&j(N,t,f(N).keyMap),r===b?null:b}),S=function(t,e,n){var r=f(t);return e===r.nodeName&&n==r.key},E=function(t,e){if(!b||!S(b,t,e)){var n=f(N),r=b&&f(b),i=n.keyMap,o=void 0;if(e){var a=i[e];a&&(S(a,t,e)?o=a:a===b?w.markDeleted(a):j(N,a,i))}o||(o="#text"===t?p(C):d(C,N,t,e),e&&(i[e]=o),w.markCreated(o)),f(o).focused?x(N,o,b):r&&r.key&&!r.focused?(N.replaceChild(o,b),n.keyMapValid=!1):N.insertBefore(o,b),b=o}},j=function(t,e,n){t.removeChild(e),w.markDeleted(e);var r=f(e).key;r&&delete n[r]},I=function(){var t=N,e=f(t),n=e.keyMap,r=e.keyMapValid,i=t.lastChild,o=void 0;if(i!==b||!r){for(;i!==b;)j(t,i,n),i=t.lastChild;if(!r){for(o in n)i=n[o],i.parentNode!==t&&(w.markDeleted(i),delete n[o]);e.keyMapValid=!0}}},V=function(){N=b,b=null},P=function(){return b?b.nextSibling:N.firstChild},_=function(){b=P()},T=function(){I(),b=N,N=N.parentNode},B=function(t,e){return _(),E(t,e),V(),N},F=function(){return T(),b},L=function(){return _(),E("#text",null),b},R=function(){return N},U=function(){return P()},X=function(){b=N.lastChild},q=_,z={"default":"__default"},G=function(t){return 0===t.lastIndexOf("xml:",0)?"http://www.w3.org/XML/1998/namespace":0===t.lastIndexOf("xlink:",0)?"http://www.w3.org/1999/xlink":void 0},H=function(t,e,n){if(null==n)t.removeAttribute(e);else{var r=G(e);r?t.setAttributeNS(r,e,n):t.setAttribute(e,n)}},J=function(t,e,n){t[e]=n},K=function(t,e,n){e.indexOf("-")>=0?t.setProperty(e,n):t[e]=n},Q=function(t,e,n){if("string"==typeof n)t.style.cssText=n;else{t.style.cssText="";var r=t.style,i=n;for(var a in i)o(i,a)&&K(r,a,i[a])}},W=function(t,e,n){var r=typeof n;"object"===r||"function"===r?J(t,e,n):H(t,e,n)},Y=function(t,e,n){var r=f(t),i=r.attrs;if(i[e]!==n){var o=Z[e]||Z[z["default"]];o(t,e,n),i[e]=n}},Z=a();Z[z["default"]]=W,Z.style=Q;var $=3,tt=[],et=function(t,e,n){var r=B(t,e),i=f(r);if(!i.staticsApplied){if(n)for(var o=0;o<n.length;o+=2){var a=n[o],u=n[o+1];Y(r,a,u)}i.staticsApplied=!0}for(var l=i.attrsArr,c=i.newAttrs,s=!l.length,d=$,p=0;d<arguments.length;d+=2,p+=2){var h=arguments[d];if(s)l[p]=h,c[h]=void 0;else if(l[p]!==h)break;var u=arguments[d+1];(s||l[p+1]!==u)&&(l[p+1]=u,Y(r,h,u))}if(d<arguments.length||p<l.length){for(;d<arguments.length;d+=1,p+=1)l[p]=arguments[d];for(p<l.length&&(l.length=p),d=0;d<l.length;d+=2){var a=l[d],u=l[d+1];c[a]=u}for(var v in c)Y(r,v,c[v]),c[v]=void 0}return r},nt=function(t,e,n){tt[0]=t,tt[1]=e,tt[2]=n},rt=function(t,e){tt.push(t),tt.push(e)},it=function(){var t=et.apply(null,tt);return tt.length=0,t},ot=function(){var t=F();return t},at=function(t){return et.apply(null,arguments),ot(t)},ut=function(t){var e=L(),n=f(e);if(n.text!==t){n.text=t;for(var r=t,i=1;i<arguments.length;i+=1){var o=arguments[i];r=o(r)}e.data=r}return e};t.patch=O,t.patchInner=O,t.patchOuter=M,t.currentElement=R,t.currentPointer=U,t.skip=X,t.skipNode=q,t.elementVoid=at,t.elementOpenStart=nt,t.elementOpenEnd=it,t.elementOpen=et,t.elementClose=ot,t.text=ut,t.attr=rt,t.symbols=z,t.attributes=Z,t.applyAttr=H,t.applyProp=J,t.notifications=h,t.importNode=c});

//# sourceMappingURL=incremental-dom-min.js.map
var jsonml2idom = (function () {
	"use strict";

	var elementOpenStart = IncrementalDOM.elementOpenStart
	var elementOpenEnd = IncrementalDOM.elementOpenEnd
	var elementClose = IncrementalDOM.elementClose
	var currentElement = IncrementalDOM.currentElement
	var skip = IncrementalDOM.skip
	var attr = IncrementalDOM.attr
	var text = IncrementalDOM.text

	function openTag(head, keyAttr) {
		var dotSplit = head.split('.')
		var hashSplit = dotSplit[0].split('#')

		var tagName = hashSplit[0] || 'div'
		var id = hashSplit[1]
		var className = dotSplit.slice(1).join(' ')

		elementOpenStart(tagName, keyAttr)

		if (id) attr('id', id)
		if (className) attr('class', className)

		return tagName
	}

	function applyAttrsObj(attrsObj) {
		for (var k in attrsObj) {
			attr(k, attrsObj[k])
		}
	}

	function parse(markup) {
		var head = markup[0]
		var attrsObj = markup[1]
		var hasAttrs = attrsObj && attrsObj.constructor === Object
		var firstChildPos = hasAttrs ? 2 : 1
		var keyAttr = hasAttrs && attrsObj.key
		var skipAttr = hasAttrs && attrsObj.skip

		var tagName = openTag(head, keyAttr)

		if (hasAttrs) applyAttrsObj(attrsObj)

		elementOpenEnd()

		if (skipAttr) {
			skip()
		} else {
			for (var i = firstChildPos, len = markup.length; i < len; i++) {
				var node = markup[i]

				if (node === undefined) continue

				switch (node.constructor) {
					case Array:
						parse(node)
						break
					case Function:
						node(currentElement())
						break
					default:
						text(node)
				}
			}
		}

		elementClose(tagName)
	}

	return parse
})();

/**
 * marked - a markdown parser
 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
 * https://github.com/markedjs/marked
 */
!function(e){"use strict";var t={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:d,hr:/^ {0,3}((?:- *){3,}|(?:_ *){3,}|(?:\* *){3,})(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *(?:#+ *)?(?:\n+|$)/,nptable:d,blockquote:/^( {0,3}> ?(paragraph|[^\n]*)(?:\n|$))+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:"^ {0,3}(?:<(script|pre|style)[\\s>][\\s\\S]*?(?:</\\1>[^\\n]*\\n+|$)|comment[^\\n]*(\\n+|$)|<\\?[\\s\\S]*?\\?>\\n*|<![A-Z][\\s\\S]*?>\\n*|<!\\[CDATA\\[[\\s\\S]*?\\]\\]>\\n*|</?(tag)(?: +|\\n|/?>)[\\s\\S]*?(?:\\n{2,}|$)|<(?!script|pre|style)([a-z][\\w-]*)(?:attribute)*? */?>(?=\\h*\\n)[\\s\\S]*?(?:\\n{2,}|$)|</(?!script|pre|style)[a-z][\\w-]*\\s*>(?=\\h*\\n)[\\s\\S]*?(?:\\n{2,}|$))",def:/^ {0,3}\[(label)\]: *\n? *<?([^\s>]+)>?(?:(?: +\n? *| *\n *)(title))? *(?:\n+|$)/,table:d,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,paragraph:/^([^\n]+(?:\n(?!hr|heading|lheading| {0,3}>|<\/?(?:tag)(?: +|\n|\/?>)|<(?:script|pre|style|!--))[^\n]+)*)/,text:/^[^\n]+/};function n(e){this.tokens=[],this.tokens.links={},this.options=e||m.defaults,this.rules=t.normal,this.options.pedantic?this.rules=t.pedantic:this.options.gfm&&(this.options.tables?this.rules=t.tables:this.rules=t.gfm)}t._label=/(?!\s*\])(?:\\[\[\]]|[^\[\]])+/,t._title=/(?:"(?:\\"?|[^"\\])*"|'[^'\n]*(?:\n[^'\n]+)*\n?'|\([^()]*\))/,t.def=p(t.def).replace("label",t._label).replace("title",t._title).getRegex(),t.bullet=/(?:[*+-]|\d+\.)/,t.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/,t.item=p(t.item,"gm").replace(/bull/g,t.bullet).getRegex(),t.list=p(t.list).replace(/bull/g,t.bullet).replace("hr","\\n+(?=\\1?(?:(?:- *){3,}|(?:_ *){3,}|(?:\\* *){3,})(?:\\n+|$))").replace("def","\\n+(?="+t.def.source+")").getRegex(),t._tag="address|article|aside|base|basefont|blockquote|body|caption|center|col|colgroup|dd|details|dialog|dir|div|dl|dt|fieldset|figcaption|figure|footer|form|frame|frameset|h[1-6]|head|header|hr|html|iframe|legend|li|link|main|menu|menuitem|meta|nav|noframes|ol|optgroup|option|p|param|section|source|summary|table|tbody|td|tfoot|th|thead|title|tr|track|ul",t._comment=/<!--(?!-?>)[\s\S]*?-->/,t.html=p(t.html,"i").replace("comment",t._comment).replace("tag",t._tag).replace("attribute",/ +[a-zA-Z:_][\w.:-]*(?: *= *"[^"\n]*"| *= *'[^'\n]*'| *= *[^\s"'=<>`]+)?/).getRegex(),t.paragraph=p(t.paragraph).replace("hr",t.hr).replace("heading",t.heading).replace("lheading",t.lheading).replace("tag",t._tag).getRegex(),t.blockquote=p(t.blockquote).replace("paragraph",t.paragraph).getRegex(),t.normal=f({},t),t.gfm=f({},t.normal,{fences:/^ *(`{3,}|~{3,})[ \.]*(\S+)? *\n([\s\S]*?)\n? *\1 *(?:\n+|$)/,paragraph:/^/,heading:/^ *(#{1,6}) +([^\n]+?) *#* *(?:\n+|$)/}),t.gfm.paragraph=p(t.paragraph).replace("(?!","(?!"+t.gfm.fences.source.replace("\\1","\\2")+"|"+t.list.source.replace("\\1","\\3")+"|").getRegex(),t.tables=f({},t.gfm,{nptable:/^ *([^|\n ].*\|.*)\n *([-:]+ *\|[-| :]*)(?:\n((?:.*[^>\n ].*(?:\n|$))*)\n*|$)/,table:/^ *\|(.+)\n *\|?( *[-:]+[-| :]*)(?:\n((?: *[^>\n ].*(?:\n|$))*)\n*|$)/}),t.pedantic=f({},t.normal,{html:p("^ *(?:comment *(?:\\n|\\s*$)|<(tag)[\\s\\S]+?</\\1> *(?:\\n{2,}|\\s*$)|<tag(?:\"[^\"]*\"|'[^']*'|\\s[^'\"/>\\s]*)*?/?> *(?:\\n{2,}|\\s*$))").replace("comment",t._comment).replace(/tag/g,"(?!(?:a|em|strong|small|s|cite|q|dfn|abbr|data|time|code|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo|span|br|wbr|ins|del|img)\\b)\\w+(?!:|[^\\w\\s@]*@)\\b").getRegex(),def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +(["(][^\n]+[")]))? *(?:\n+|$)/}),n.rules=t,n.lex=function(e,t){return new n(t).lex(e)},n.prototype.lex=function(e){return e=e.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n"),this.token(e,!0)},n.prototype.token=function(e,n){var r,s,i,l,o,a,h,p,u,c,g,d,f;for(e=e.replace(/^ +$/gm,"");e;)if((i=this.rules.newline.exec(e))&&(e=e.substring(i[0].length),i[0].length>1&&this.tokens.push({type:"space"})),i=this.rules.code.exec(e))e=e.substring(i[0].length),i=i[0].replace(/^ {4}/gm,""),this.tokens.push({type:"code",text:this.options.pedantic?i:i.replace(/\n+$/,"")});else if(i=this.rules.fences.exec(e))e=e.substring(i[0].length),this.tokens.push({type:"code",lang:i[2],text:i[3]||""});else if(i=this.rules.heading.exec(e))e=e.substring(i[0].length),this.tokens.push({type:"heading",depth:i[1].length,text:i[2]});else if(n&&(i=this.rules.nptable.exec(e))&&(a={type:"table",header:b(i[1].replace(/^ *| *\| *$/g,"")),align:i[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:i[3]?i[3].replace(/\n$/,"").split("\n"):[]}).header.length===a.align.length){for(e=e.substring(i[0].length),p=0;p<a.align.length;p++)/^ *-+: *$/.test(a.align[p])?a.align[p]="right":/^ *:-+: *$/.test(a.align[p])?a.align[p]="center":/^ *:-+ *$/.test(a.align[p])?a.align[p]="left":a.align[p]=null;for(p=0;p<a.cells.length;p++)a.cells[p]=b(a.cells[p],a.header.length);this.tokens.push(a)}else if(i=this.rules.hr.exec(e))e=e.substring(i[0].length),this.tokens.push({type:"hr"});else if(i=this.rules.blockquote.exec(e))e=e.substring(i[0].length),this.tokens.push({type:"blockquote_start"}),i=i[0].replace(/^ *> ?/gm,""),this.token(i,n),this.tokens.push({type:"blockquote_end"});else if(i=this.rules.list.exec(e)){for(e=e.substring(i[0].length),g=(l=i[2]).length>1,this.tokens.push({type:"list_start",ordered:g,start:g?+l:""}),r=!1,c=(i=i[0].match(this.rules.item)).length,p=0;p<c;p++)h=(a=i[p]).length,~(a=a.replace(/^ *([*+-]|\d+\.) +/,"")).indexOf("\n ")&&(h-=a.length,a=this.options.pedantic?a.replace(/^ {1,4}/gm,""):a.replace(new RegExp("^ {1,"+h+"}","gm"),"")),this.options.smartLists&&p!==c-1&&(l===(o=t.bullet.exec(i[p+1])[0])||l.length>1&&o.length>1||(e=i.slice(p+1).join("\n")+e,p=c-1)),s=r||/\n\n(?!\s*$)/.test(a),p!==c-1&&(r="\n"===a.charAt(a.length-1),s||(s=r)),f=void 0,(d=/^\[[ xX]\] /.test(a))&&(f=" "!==a[1],a=a.replace(/^\[[ xX]\] +/,"")),this.tokens.push({type:s?"loose_item_start":"list_item_start",task:d,checked:f}),this.token(a,!1),this.tokens.push({type:"list_item_end"});this.tokens.push({type:"list_end"})}else if(i=this.rules.html.exec(e))e=e.substring(i[0].length),this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:!this.options.sanitizer&&("pre"===i[1]||"script"===i[1]||"style"===i[1]),text:i[0]});else if(n&&(i=this.rules.def.exec(e)))e=e.substring(i[0].length),i[3]&&(i[3]=i[3].substring(1,i[3].length-1)),u=i[1].toLowerCase().replace(/\s+/g," "),this.tokens.links[u]||(this.tokens.links[u]={href:i[2],title:i[3]});else if(n&&(i=this.rules.table.exec(e))&&(a={type:"table",header:b(i[1].replace(/^ *| *\| *$/g,"")),align:i[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:i[3]?i[3].replace(/(?: *\| *)?\n$/,"").split("\n"):[]}).header.length===a.align.length){for(e=e.substring(i[0].length),p=0;p<a.align.length;p++)/^ *-+: *$/.test(a.align[p])?a.align[p]="right":/^ *:-+: *$/.test(a.align[p])?a.align[p]="center":/^ *:-+ *$/.test(a.align[p])?a.align[p]="left":a.align[p]=null;for(p=0;p<a.cells.length;p++)a.cells[p]=b(a.cells[p].replace(/^ *\| *| *\| *$/g,""),a.header.length);this.tokens.push(a)}else if(i=this.rules.lheading.exec(e))e=e.substring(i[0].length),this.tokens.push({type:"heading",depth:"="===i[2]?1:2,text:i[1]});else if(n&&(i=this.rules.paragraph.exec(e)))e=e.substring(i[0].length),this.tokens.push({type:"paragraph",text:"\n"===i[1].charAt(i[1].length-1)?i[1].slice(0,-1):i[1]});else if(i=this.rules.text.exec(e))e=e.substring(i[0].length),this.tokens.push({type:"text",text:i[0]});else if(e)throw new Error("Infinite loop on byte: "+e.charCodeAt(0));return this.tokens};var r={escape:/^\\([!"#$%&'()*+,\-./:;<=>?@\[\]\\^_`{|}~])/,autolink:/^<(scheme:[^\s\x00-\x1f<>]*|email)>/,url:d,tag:"^comment|^</[a-zA-Z][\\w:-]*\\s*>|^<[a-zA-Z][\\w-]*(?:attribute)*?\\s*/?>|^<\\?[\\s\\S]*?\\?>|^<![a-zA-Z]+\\s[\\s\\S]*?>|^<!\\[CDATA\\[[\\s\\S]*?\\]\\]>",link:/^!?\[(label)\]\(href(?:\s+(title))?\s*\)/,reflink:/^!?\[(label)\]\[(?!\s*\])((?:\\[\[\]]?|[^\[\]\\])+)\]/,nolink:/^!?\[(?!\s*\])((?:\[[^\[\]]*\]|\\[\[\]]|[^\[\]])*)\](?:\[\])?/,strong:/^__([^\s][\s\S]*?[^\s])__(?!_)|^\*\*([^\s][\s\S]*?[^\s])\*\*(?!\*)|^__([^\s])__(?!_)|^\*\*([^\s])\*\*(?!\*)/,em:/^_([^\s][\s\S]*?[^\s_])_(?!_)|^_([^\s_][\s\S]*?[^\s])_(?!_)|^\*([^\s][\s\S]*?[^\s*])\*(?!\*)|^\*([^\s*][\s\S]*?[^\s])\*(?!\*)|^_([^\s_])_(?!_)|^\*([^\s*])\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`]?)\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:d,text:/^[\s\S]+?(?=[\\<!\[`*]|\b_| {2,}\n|$)/};function s(e,t){if(this.options=t||m.defaults,this.links=e,this.rules=r.normal,this.renderer=this.options.renderer||new i,this.renderer.options=this.options,!this.links)throw new Error("Tokens array requires a `links` property.");this.options.pedantic?this.rules=r.pedantic:this.options.gfm&&(this.options.breaks?this.rules=r.breaks:this.rules=r.gfm)}function i(e){this.options=e||m.defaults}function l(){}function o(e){this.tokens=[],this.token=null,this.options=e||m.defaults,this.options.renderer=this.options.renderer||new i,this.renderer=this.options.renderer,this.renderer.options=this.options}function a(e,t){return e.replace(t?/&/g:/&(?!#?\w+;)/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function h(e){return e.replace(/&(#(?:\d+)|(?:#x[0-9A-Fa-f]+)|(?:\w+));?/gi,function(e,t){return"colon"===(t=t.toLowerCase())?":":"#"===t.charAt(0)?"x"===t.charAt(1)?String.fromCharCode(parseInt(t.substring(2),16)):String.fromCharCode(+t.substring(1)):""})}function p(e,t){return e=e.source||e,t=t||"",{replace:function(t,n){return n=(n=n.source||n).replace(/(^|[^\[])\^/g,"$1"),e=e.replace(t,n),this},getRegex:function(){return new RegExp(e,t)}}}function u(e,t){return c[" "+e]||(/^[^:]+:\/*[^/]*$/.test(e)?c[" "+e]=e+"/":c[" "+e]=e.replace(/[^/]*$/,"")),e=c[" "+e],"//"===t.slice(0,2)?e.replace(/:[\s\S]*/,":")+t:"/"===t.charAt(0)?e.replace(/(:\/*[^/]*)[\s\S]*/,"$1")+t:e+t}r._escapes=/\\([!"#$%&'()*+,\-./:;<=>?@\[\]\\^_`{|}~])/g,r._scheme=/[a-zA-Z][a-zA-Z0-9+.-]{1,31}/,r._email=/[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+(@)[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+(?![-_])/,r.autolink=p(r.autolink).replace("scheme",r._scheme).replace("email",r._email).getRegex(),r._attribute=/\s+[a-zA-Z:_][\w.:-]*(?:\s*=\s*"[^"]*"|\s*=\s*'[^']*'|\s*=\s*[^\s"'=<>`]+)?/,r.tag=p(r.tag).replace("comment",t._comment).replace("attribute",r._attribute).getRegex(),r._label=/(?:\[[^\[\]]*\]|\\[\[\]]?|`[^`]*`|[^\[\]\\])*?/,r._href=/\s*(<(?:\\[<>]?|[^\s<>\\])*>|(?:\\[()]?|\([^\s\x00-\x1f()\\]*\)|[^\s\x00-\x1f()\\])*?)/,r._title=/"(?:\\"?|[^"\\])*"|'(?:\\'?|[^'\\])*'|\((?:\\\)?|[^)\\])*\)/,r.link=p(r.link).replace("label",r._label).replace("href",r._href).replace("title",r._title).getRegex(),r.reflink=p(r.reflink).replace("label",r._label).getRegex(),r.normal=f({},r),r.pedantic=f({},r.normal,{strong:/^__(?=\S)([\s\S]*?\S)__(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/,link:p(/^!?\[(label)\]\((.*?)\)/).replace("label",r._label).getRegex(),reflink:p(/^!?\[(label)\]\s*\[([^\]]*)\]/).replace("label",r._label).getRegex()}),r.gfm=f({},r.normal,{escape:p(r.escape).replace("])","~|])").getRegex(),url:p(/^((?:ftp|https?):\/\/|www\.)(?:[a-zA-Z0-9\-]+\.?)+[^\s<]*|^email/).replace("email",r._email).getRegex(),_backpedal:/(?:[^?!.,:;*_~()&]+|\([^)]*\)|&(?![a-zA-Z0-9]+;$)|[?!.,:;*_~)]+(?!$))+/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:p(r.text).replace("]|","~]|").replace("|","|https?://|ftp://|www\\.|[a-zA-Z0-9.!#$%&'*+/=?^_`{\\|}~-]+@|").getRegex()}),r.breaks=f({},r.gfm,{br:p(r.br).replace("{2,}","*").getRegex(),text:p(r.gfm.text).replace("{2,}","*").getRegex()}),s.rules=r,s.output=function(e,t,n){return new s(t,n).output(e)},s.prototype.output=function(e){for(var t,n,r,i,l,o="";e;)if(l=this.rules.escape.exec(e))e=e.substring(l[0].length),o+=l[1];else if(l=this.rules.autolink.exec(e))e=e.substring(l[0].length),r="@"===l[2]?"mailto:"+(n=a(this.mangle(l[1]))):n=a(l[1]),o+=this.renderer.link(r,null,n);else if(this.inLink||!(l=this.rules.url.exec(e))){if(l=this.rules.tag.exec(e))!this.inLink&&/^<a /i.test(l[0])?this.inLink=!0:this.inLink&&/^<\/a>/i.test(l[0])&&(this.inLink=!1),e=e.substring(l[0].length),o+=this.options.sanitize?this.options.sanitizer?this.options.sanitizer(l[0]):a(l[0]):l[0];else if(l=this.rules.link.exec(e))e=e.substring(l[0].length),this.inLink=!0,r=l[2],this.options.pedantic?(t=/^([^'"]*[^\s])\s+(['"])(.*)\2/.exec(r))?(r=t[1],i=t[3]):i="":i=l[3]?l[3].slice(1,-1):"",r=r.trim().replace(/^<([\s\S]*)>$/,"$1"),o+=this.outputLink(l,{href:s.escapes(r),title:s.escapes(i)}),this.inLink=!1;else if((l=this.rules.reflink.exec(e))||(l=this.rules.nolink.exec(e))){if(e=e.substring(l[0].length),t=(l[2]||l[1]).replace(/\s+/g," "),!(t=this.links[t.toLowerCase()])||!t.href){o+=l[0].charAt(0),e=l[0].substring(1)+e;continue}this.inLink=!0,o+=this.outputLink(l,t),this.inLink=!1}else if(l=this.rules.strong.exec(e))e=e.substring(l[0].length),o+=this.renderer.strong(this.output(l[4]||l[3]||l[2]||l[1]));else if(l=this.rules.em.exec(e))e=e.substring(l[0].length),o+=this.renderer.em(this.output(l[6]||l[5]||l[4]||l[3]||l[2]||l[1]));else if(l=this.rules.code.exec(e))e=e.substring(l[0].length),o+=this.renderer.codespan(a(l[2].trim(),!0));else if(l=this.rules.br.exec(e))e=e.substring(l[0].length),o+=this.renderer.br();else if(l=this.rules.del.exec(e))e=e.substring(l[0].length),o+=this.renderer.del(this.output(l[1]));else if(l=this.rules.text.exec(e))e=e.substring(l[0].length),o+=this.renderer.text(a(this.smartypants(l[0])));else if(e)throw new Error("Infinite loop on byte: "+e.charCodeAt(0))}else l[0]=this.rules._backpedal.exec(l[0])[0],e=e.substring(l[0].length),"@"===l[2]?r="mailto:"+(n=a(l[0])):(n=a(l[0]),r="www."===l[1]?"http://"+n:n),o+=this.renderer.link(r,null,n);return o},s.escapes=function(e){return e?e.replace(s.rules._escapes,"$1"):e},s.prototype.outputLink=function(e,t){var n=t.href,r=t.title?a(t.title):null;return"!"!==e[0].charAt(0)?this.renderer.link(n,r,this.output(e[1])):this.renderer.image(n,r,a(e[1]))},s.prototype.smartypants=function(e){return this.options.smartypants?e.replace(/---/g,"—").replace(/--/g,"–").replace(/(^|[-\u2014/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…"):e},s.prototype.mangle=function(e){if(!this.options.mangle)return e;for(var t,n="",r=e.length,s=0;s<r;s++)t=e.charCodeAt(s),Math.random()>.5&&(t="x"+t.toString(16)),n+="&#"+t+";";return n},i.prototype.code=function(e,t,n){if(this.options.highlight){var r=this.options.highlight(e,t);null!=r&&r!==e&&(n=!0,e=r)}return t?'<pre><code class="'+this.options.langPrefix+a(t,!0)+'">'+(n?e:a(e,!0))+"</code></pre>\n":"<pre><code>"+(n?e:a(e,!0))+"</code></pre>"},i.prototype.blockquote=function(e){return"<blockquote>\n"+e+"</blockquote>\n"},i.prototype.html=function(e){return e},i.prototype.heading=function(e,t,n){return this.options.headerIds?"<h"+t+' id="'+this.options.headerPrefix+n.toLowerCase().replace(/[^\w]+/g,"-")+'">'+e+"</h"+t+">\n":"<h"+t+">"+e+"</h"+t+">\n"},i.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"},i.prototype.list=function(e,t,n){var r=t?"ol":"ul";return"<"+r+(t&&1!==n?' start="'+n+'"':"")+">\n"+e+"</"+r+">\n"},i.prototype.listitem=function(e){return"<li>"+e+"</li>\n"},i.prototype.checkbox=function(e){return"<input "+(e?'checked="" ':"")+'disabled="" type="checkbox"'+(this.options.xhtml?" /":"")+"> "},i.prototype.paragraph=function(e){return"<p>"+e+"</p>\n"},i.prototype.table=function(e,t){return t&&(t="<tbody>"+t+"</tbody>"),"<table>\n<thead>\n"+e+"</thead>\n"+t+"</table>\n"},i.prototype.tablerow=function(e){return"<tr>\n"+e+"</tr>\n"},i.prototype.tablecell=function(e,t){var n=t.header?"th":"td";return(t.align?"<"+n+' align="'+t.align+'">':"<"+n+">")+e+"</"+n+">\n"},i.prototype.strong=function(e){return"<strong>"+e+"</strong>"},i.prototype.em=function(e){return"<em>"+e+"</em>"},i.prototype.codespan=function(e){return"<code>"+e+"</code>"},i.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"},i.prototype.del=function(e){return"<del>"+e+"</del>"},i.prototype.link=function(e,t,n){if(this.options.sanitize){try{var r=decodeURIComponent(h(e)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return n}if(0===r.indexOf("javascript:")||0===r.indexOf("vbscript:")||0===r.indexOf("data:"))return n}this.options.baseUrl&&!g.test(e)&&(e=u(this.options.baseUrl,e));try{e=encodeURI(e).replace(/%25/g,"%")}catch(e){return n}var s='<a href="'+a(e)+'"';return t&&(s+=' title="'+t+'"'),s+=">"+n+"</a>"},i.prototype.image=function(e,t,n){this.options.baseUrl&&!g.test(e)&&(e=u(this.options.baseUrl,e));var r='<img src="'+e+'" alt="'+n+'"';return t&&(r+=' title="'+t+'"'),r+=this.options.xhtml?"/>":">"},i.prototype.text=function(e){return e},l.prototype.strong=l.prototype.em=l.prototype.codespan=l.prototype.del=l.prototype.text=function(e){return e},l.prototype.link=l.prototype.image=function(e,t,n){return""+n},l.prototype.br=function(){return""},o.parse=function(e,t){return new o(t).parse(e)},o.prototype.parse=function(e){this.inline=new s(e.links,this.options),this.inlineText=new s(e.links,f({},this.options,{renderer:new l})),this.tokens=e.reverse();for(var t="";this.next();)t+=this.tok();return t},o.prototype.next=function(){return this.token=this.tokens.pop()},o.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0},o.prototype.parseText=function(){for(var e=this.token.text;"text"===this.peek().type;)e+="\n"+this.next().text;return this.inline.output(e)},o.prototype.tok=function(){switch(this.token.type){case"space":return"";case"hr":return this.renderer.hr();case"heading":return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,h(this.inlineText.output(this.token.text)));case"code":return this.renderer.code(this.token.text,this.token.lang,this.token.escaped);case"table":var e,t,n,r,s="",i="";for(n="",e=0;e<this.token.header.length;e++)n+=this.renderer.tablecell(this.inline.output(this.token.header[e]),{header:!0,align:this.token.align[e]});for(s+=this.renderer.tablerow(n),e=0;e<this.token.cells.length;e++){for(t=this.token.cells[e],n="",r=0;r<t.length;r++)n+=this.renderer.tablecell(this.inline.output(t[r]),{header:!1,align:this.token.align[r]});i+=this.renderer.tablerow(n)}return this.renderer.table(s,i);case"blockquote_start":for(i="";"blockquote_end"!==this.next().type;)i+=this.tok();return this.renderer.blockquote(i);case"list_start":i="";for(var l=this.token.ordered,o=this.token.start;"list_end"!==this.next().type;)i+=this.tok();return this.renderer.list(i,l,o);case"list_item_start":for(i="",this.token.task&&(i+=this.renderer.checkbox(this.token.checked));"list_item_end"!==this.next().type;)i+="text"===this.token.type?this.parseText():this.tok();return this.renderer.listitem(i);case"loose_item_start":for(i="";"list_item_end"!==this.next().type;)i+=this.tok();return this.renderer.listitem(i);case"html":return this.renderer.html(this.token.text);case"paragraph":return this.renderer.paragraph(this.inline.output(this.token.text));case"text":return this.renderer.paragraph(this.parseText())}};var c={},g=/^$|^[a-z][a-z0-9+.-]*:|^[?#]/i;function d(){}function f(e){for(var t,n,r=1;r<arguments.length;r++)for(n in t=arguments[r])Object.prototype.hasOwnProperty.call(t,n)&&(e[n]=t[n]);return e}function b(e,t){var n=e.replace(/([^\\])\|/g,"$1 |").split(/ +\| */),r=0;if(n.length>t)n.splice(t);else for(;n.length<t;)n.push("");for(;r<n.length;r++)n[r]=n[r].replace(/\\\|/g,"|");return n}function m(e,t,r){if(null==e)throw new Error("marked(): input parameter is undefined or null");if("string"!=typeof e)throw new Error("marked(): input parameter is of type "+Object.prototype.toString.call(e)+", string expected");if(r||"function"==typeof t){r||(r=t,t=null);var s,i,l=(t=f({},m.defaults,t||{})).highlight,h=0;try{s=n.lex(e,t)}catch(e){return r(e)}i=s.length;var p=function(e){if(e)return t.highlight=l,r(e);var n;try{n=o.parse(s,t)}catch(t){e=t}return t.highlight=l,e?r(e):r(null,n)};if(!l||l.length<3)return p();if(delete t.highlight,!i)return p();for(;h<s.length;h++)!function(e){"code"!==e.type?--i||p():l(e.text,e.lang,function(t,n){return t?p(t):null==n||n===e.text?--i||p():(e.text=n,e.escaped=!0,void(--i||p()))})}(s[h])}else try{return t&&(t=f({},m.defaults,t)),o.parse(n.lex(e,t),t)}catch(e){if(e.message+="\nPlease report this to https://github.com/markedjs/marked.",(t||m.defaults).silent)return"<p>An error occurred:</p><pre>"+a(e.message+"",!0)+"</pre>";throw e}}d.exec=d,m.options=m.setOptions=function(e){return f(m.defaults,e),m},m.getDefaults=function(){return{baseUrl:null,breaks:!1,gfm:!0,headerIds:!0,headerPrefix:"",highlight:null,langPrefix:"language-",mangle:!0,pedantic:!1,renderer:new i,sanitize:!1,sanitizer:null,silent:!1,smartLists:!1,smartypants:!1,tables:!0,xhtml:!1}},m.defaults=m.getDefaults(),m.Parser=o,m.parser=o.parse,m.Renderer=i,m.TextRenderer=l,m.Lexer=n,m.lexer=n.lex,m.InlineLexer=s,m.inlineLexer=s.output,m.parse=m,"undefined"!=typeof module&&"object"==typeof exports?module.exports=m:"function"==typeof define&&define.amd?define(function(){return m}):e.marked=m}(this||("undefined"!=typeof window?window:global));
(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.himalaya = f()}})(function(){var define,module,exports;return (function(){function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s}return e})()({1:[function(require,module,exports){
'use strict';

var cov_24vn3a78n4 = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/compat.js',
      hash = 'cde94accf38c67a096269c512dfb0f1bca69a38a',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/compat.js',
    statementMap: {
      '0': {
        start: {
          line: 10,
          column: 2
        },
        end: {
          line: 10,
          column: 72
        }
      },
      '1': {
        start: {
          line: 14,
          column: 16
        },
        end: {
          line: 14,
          column: 62
        }
      },
      '2': {
        start: {
          line: 15,
          column: 20
        },
        end: {
          line: 15,
          column: 56
        }
      },
      '3': {
        start: {
          line: 16,
          column: 2
        },
        end: {
          line: 16,
          column: 48
        }
      },
      '4': {
        start: {
          line: 20,
          column: 2
        },
        end: {
          line: 20,
          column: 56
        }
      },
      '5': {
        start: {
          line: 24,
          column: 2
        },
        end: {
          line: 24,
          column: 42
        }
      },
      '6': {
        start: {
          line: 28,
          column: 14
        },
        end: {
          line: 28,
          column: 26
        }
      },
      '7': {
        start: {
          line: 29,
          column: 2
        },
        end: {
          line: 29,
          column: 29
        }
      },
      '8': {
        start: {
          line: 29,
          column: 17
        },
        end: {
          line: 29,
          column: 29
        }
      },
      '9': {
        start: {
          line: 31,
          column: 22
        },
        end: {
          line: 31,
          column: 34
        }
      },
      '10': {
        start: {
          line: 32,
          column: 23
        },
        end: {
          line: 32,
          column: 47
        }
      },
      '11': {
        start: {
          line: 33,
          column: 20
        },
        end: {
          line: 33,
          column: 70
        }
      },
      '12': {
        start: {
          line: 34,
          column: 2
        },
        end: {
          line: 38,
          column: 3
        }
      },
      '13': {
        start: {
          line: 35,
          column: 20
        },
        end: {
          line: 35,
          column: 40
        }
      },
      '14': {
        start: {
          line: 36,
          column: 4
        },
        end: {
          line: 36,
          column: 46
        }
      },
      '15': {
        start: {
          line: 36,
          column: 35
        },
        end: {
          line: 36,
          column: 46
        }
      },
      '16': {
        start: {
          line: 37,
          column: 4
        },
        end: {
          line: 37,
          column: 55
        }
      },
      '17': {
        start: {
          line: 37,
          column: 44
        },
        end: {
          line: 37,
          column: 55
        }
      },
      '18': {
        start: {
          line: 40,
          column: 2
        },
        end: {
          line: 40,
          column: 14
        }
      }
    },
    fnMap: {
      '0': {
        name: 'startsWith',
        decl: {
          start: {
            line: 9,
            column: 16
          },
          end: {
            line: 9,
            column: 26
          }
        },
        loc: {
          start: {
            line: 9,
            column: 57
          },
          end: {
            line: 11,
            column: 1
          }
        },
        line: 9
      },
      '1': {
        name: 'endsWith',
        decl: {
          start: {
            line: 13,
            column: 16
          },
          end: {
            line: 13,
            column: 24
          }
        },
        loc: {
          start: {
            line: 13,
            column: 55
          },
          end: {
            line: 17,
            column: 1
          }
        },
        line: 13
      },
      '2': {
        name: 'stringIncludes',
        decl: {
          start: {
            line: 19,
            column: 16
          },
          end: {
            line: 19,
            column: 30
          }
        },
        loc: {
          start: {
            line: 19,
            column: 61
          },
          end: {
            line: 21,
            column: 1
          }
        },
        line: 19
      },
      '3': {
        name: 'isRealNaN',
        decl: {
          start: {
            line: 23,
            column: 16
          },
          end: {
            line: 23,
            column: 25
          }
        },
        loc: {
          start: {
            line: 23,
            column: 30
          },
          end: {
            line: 25,
            column: 1
          }
        },
        line: 23
      },
      '4': {
        name: 'arrayIncludes',
        decl: {
          start: {
            line: 27,
            column: 16
          },
          end: {
            line: 27,
            column: 29
          }
        },
        loc: {
          start: {
            line: 27,
            column: 63
          },
          end: {
            line: 41,
            column: 1
          }
        },
        line: 27
      }
    },
    branchMap: {
      '0': {
        loc: {
          start: {
            line: 10,
            column: 20
          },
          end: {
            line: 10,
            column: 33
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 10,
            column: 20
          },
          end: {
            line: 10,
            column: 28
          }
        }, {
          start: {
            line: 10,
            column: 32
          },
          end: {
            line: 10,
            column: 33
          }
        }],
        line: 10
      },
      '1': {
        loc: {
          start: {
            line: 14,
            column: 17
          },
          end: {
            line: 14,
            column: 39
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 14,
            column: 17
          },
          end: {
            line: 14,
            column: 25
          }
        }, {
          start: {
            line: 14,
            column: 29
          },
          end: {
            line: 14,
            column: 39
          }
        }],
        line: 14
      },
      '2': {
        loc: {
          start: {
            line: 16,
            column: 9
          },
          end: {
            line: 16,
            column: 48
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 16,
            column: 9
          },
          end: {
            line: 16,
            column: 25
          }
        }, {
          start: {
            line: 16,
            column: 29
          },
          end: {
            line: 16,
            column: 48
          }
        }],
        line: 16
      },
      '3': {
        loc: {
          start: {
            line: 20,
            column: 35
          },
          end: {
            line: 20,
            column: 48
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 20,
            column: 35
          },
          end: {
            line: 20,
            column: 43
          }
        }, {
          start: {
            line: 20,
            column: 47
          },
          end: {
            line: 20,
            column: 48
          }
        }],
        line: 20
      },
      '4': {
        loc: {
          start: {
            line: 24,
            column: 9
          },
          end: {
            line: 24,
            column: 42
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 24,
            column: 9
          },
          end: {
            line: 24,
            column: 30
          }
        }, {
          start: {
            line: 24,
            column: 34
          },
          end: {
            line: 24,
            column: 42
          }
        }],
        line: 24
      },
      '5': {
        loc: {
          start: {
            line: 29,
            column: 2
          },
          end: {
            line: 29,
            column: 29
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 29,
            column: 2
          },
          end: {
            line: 29,
            column: 29
          }
        }, {
          start: {
            line: 29,
            column: 2
          },
          end: {
            line: 29,
            column: 29
          }
        }],
        line: 29
      },
      '6': {
        loc: {
          start: {
            line: 33,
            column: 20
          },
          end: {
            line: 33,
            column: 70
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 33,
            column: 39
          },
          end: {
            line: 33,
            column: 50
          }
        }, {
          start: {
            line: 33,
            column: 53
          },
          end: {
            line: 33,
            column: 70
          }
        }],
        line: 33
      },
      '7': {
        loc: {
          start: {
            line: 36,
            column: 4
          },
          end: {
            line: 36,
            column: 46
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 36,
            column: 4
          },
          end: {
            line: 36,
            column: 46
          }
        }, {
          start: {
            line: 36,
            column: 4
          },
          end: {
            line: 36,
            column: 46
          }
        }],
        line: 36
      },
      '8': {
        loc: {
          start: {
            line: 37,
            column: 4
          },
          end: {
            line: 37,
            column: 55
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 37,
            column: 4
          },
          end: {
            line: 37,
            column: 55
          }
        }, {
          start: {
            line: 37,
            column: 4
          },
          end: {
            line: 37,
            column: 55
          }
        }],
        line: 37
      },
      '9': {
        loc: {
          start: {
            line: 37,
            column: 8
          },
          end: {
            line: 37,
            column: 42
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 37,
            column: 8
          },
          end: {
            line: 37,
            column: 20
          }
        }, {
          start: {
            line: 37,
            column: 24
          },
          end: {
            line: 37,
            column: 42
          }
        }],
        line: 37
      }
    },
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0,
      '6': 0,
      '7': 0,
      '8': 0,
      '9': 0,
      '10': 0,
      '11': 0,
      '12': 0,
      '13': 0,
      '14': 0,
      '15': 0,
      '16': 0,
      '17': 0,
      '18': 0
    },
    f: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0
    },
    b: {
      '0': [0, 0],
      '1': [0, 0],
      '2': [0, 0],
      '3': [0, 0],
      '4': [0, 0],
      '5': [0, 0],
      '6': [0, 0],
      '7': [0, 0],
      '8': [0, 0],
      '9': [0, 0]
    },
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.startsWith = startsWith;
exports.endsWith = endsWith;
exports.stringIncludes = stringIncludes;
exports.isRealNaN = isRealNaN;
exports.arrayIncludes = arrayIncludes;
/*
  We don't want to include babel-polyfill in our project.
    - Library authors should be using babel-runtime for non-global polyfilling
    - Adding babel-polyfill/-runtime increases bundle size significantly

  We will include our polyfill instance methods as regular functions.
*/

function startsWith(str, searchString, position) {
  cov_24vn3a78n4.f[0]++;
  cov_24vn3a78n4.s[0]++;

  return str.substr((cov_24vn3a78n4.b[0][0]++, position) || (cov_24vn3a78n4.b[0][1]++, 0), searchString.length) === searchString;
}

function endsWith(str, searchString, position) {
  cov_24vn3a78n4.f[1]++;

  var index = (cov_24vn3a78n4.s[1]++, ((cov_24vn3a78n4.b[1][0]++, position) || (cov_24vn3a78n4.b[1][1]++, str.length)) - searchString.length);
  var lastIndex = (cov_24vn3a78n4.s[2]++, str.lastIndexOf(searchString, index));
  cov_24vn3a78n4.s[3]++;
  return (cov_24vn3a78n4.b[2][0]++, lastIndex !== -1) && (cov_24vn3a78n4.b[2][1]++, lastIndex === index);
}

function stringIncludes(str, searchString, position) {
  cov_24vn3a78n4.f[2]++;
  cov_24vn3a78n4.s[4]++;

  return str.indexOf(searchString, (cov_24vn3a78n4.b[3][0]++, position) || (cov_24vn3a78n4.b[3][1]++, 0)) !== -1;
}

function isRealNaN(x) {
  cov_24vn3a78n4.f[3]++;
  cov_24vn3a78n4.s[5]++;

  return (cov_24vn3a78n4.b[4][0]++, typeof x === 'number') && (cov_24vn3a78n4.b[4][1]++, isNaN(x));
}

function arrayIncludes(array, searchElement, position) {
  cov_24vn3a78n4.f[4]++;

  var len = (cov_24vn3a78n4.s[6]++, array.length);
  cov_24vn3a78n4.s[7]++;
  if (len === 0) {
      cov_24vn3a78n4.b[5][0]++;
      cov_24vn3a78n4.s[8]++;
      return false;
    } else {
    cov_24vn3a78n4.b[5][1]++;
  }var lookupIndex = (cov_24vn3a78n4.s[9]++, position | 0);
  var isNaNElement = (cov_24vn3a78n4.s[10]++, isRealNaN(searchElement));
  var searchIndex = (cov_24vn3a78n4.s[11]++, lookupIndex >= 0 ? (cov_24vn3a78n4.b[6][0]++, lookupIndex) : (cov_24vn3a78n4.b[6][1]++, len + lookupIndex));
  cov_24vn3a78n4.s[12]++;
  while (searchIndex < len) {
    var element = (cov_24vn3a78n4.s[13]++, array[searchIndex++]);
    cov_24vn3a78n4.s[14]++;
    if (element === searchElement) {
        cov_24vn3a78n4.b[7][0]++;
        cov_24vn3a78n4.s[15]++;
        return true;
      } else {
      cov_24vn3a78n4.b[7][1]++;
    }cov_24vn3a78n4.s[16]++;
    if ((cov_24vn3a78n4.b[9][0]++, isNaNElement) && (cov_24vn3a78n4.b[9][1]++, isRealNaN(element))) {
        cov_24vn3a78n4.b[8][0]++;
        cov_24vn3a78n4.s[17]++;
        return true;
      } else {
      cov_24vn3a78n4.b[8][1]++;
    }
  }

  cov_24vn3a78n4.s[18]++;
  return false;
}

},{}],2:[function(require,module,exports){
'use strict';

var cov_1xnzystgba = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/format.js',
      hash = 'ef8c4d14fa58c2bce23a58bf5d7c370846a07329',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/format.js',
    statementMap: {
      '0': {
        start: {
          line: 2,
          column: 14
        },
        end: {
          line: 2,
          column: 30
        }
      },
      '1': {
        start: {
          line: 3,
          column: 2
        },
        end: {
          line: 3,
          column: 30
        }
      },
      '2': {
        start: {
          line: 3,
          column: 18
        },
        end: {
          line: 3,
          column: 30
        }
      },
      '3': {
        start: {
          line: 4,
          column: 2
        },
        end: {
          line: 4,
          column: 57
        }
      },
      '4': {
        start: {
          line: 8,
          column: 14
        },
        end: {
          line: 8,
          column: 27
        }
      },
      '5': {
        start: {
          line: 9,
          column: 14
        },
        end: {
          line: 9,
          column: 28
        }
      },
      '6': {
        start: {
          line: 10,
          column: 23
        },
        end: {
          line: 10,
          column: 49
        }
      },
      '7': {
        start: {
          line: 11,
          column: 2
        },
        end: {
          line: 13,
          column: 3
        }
      },
      '8': {
        start: {
          line: 12,
          column: 4
        },
        end: {
          line: 12,
          column: 28
        }
      },
      '9': {
        start: {
          line: 14,
          column: 2
        },
        end: {
          line: 14,
          column: 12
        }
      },
      '10': {
        start: {
          line: 18,
          column: 2
        },
        end: {
          line: 32,
          column: 4
        }
      },
      '11': {
        start: {
          line: 19,
          column: 17
        },
        end: {
          line: 19,
          column: 26
        }
      },
      '12': {
        start: {
          line: 20,
          column: 23
        },
        end: {
          line: 27,
          column: 39
        }
      },
      '13': {
        start: {
          line: 28,
          column: 4
        },
        end: {
          line: 30,
          column: 5
        }
      },
      '14': {
        start: {
          line: 29,
          column: 6
        },
        end: {
          line: 29,
          column: 41
        }
      },
      '15': {
        start: {
          line: 31,
          column: 4
        },
        end: {
          line: 31,
          column: 21
        }
      },
      '16': {
        start: {
          line: 36,
          column: 2
        },
        end: {
          line: 43,
          column: 4
        }
      },
      '17': {
        start: {
          line: 37,
          column: 18
        },
        end: {
          line: 37,
          column: 50
        }
      },
      '18': {
        start: {
          line: 38,
          column: 16
        },
        end: {
          line: 38,
          column: 24
        }
      },
      '19': {
        start: {
          line: 39,
          column: 18
        },
        end: {
          line: 41,
          column: 12
        }
      },
      '20': {
        start: {
          line: 42,
          column: 4
        },
        end: {
          line: 42,
          column: 23
        }
      }
    },
    fnMap: {
      '0': {
        name: 'splitHead',
        decl: {
          start: {
            line: 1,
            column: 16
          },
          end: {
            line: 1,
            column: 25
          }
        },
        loc: {
          start: {
            line: 1,
            column: 37
          },
          end: {
            line: 5,
            column: 1
          }
        },
        line: 1
      },
      '1': {
        name: 'unquote',
        decl: {
          start: {
            line: 7,
            column: 16
          },
          end: {
            line: 7,
            column: 23
          }
        },
        loc: {
          start: {
            line: 7,
            column: 30
          },
          end: {
            line: 15,
            column: 1
          }
        },
        line: 7
      },
      '2': {
        name: 'format',
        decl: {
          start: {
            line: 17,
            column: 16
          },
          end: {
            line: 17,
            column: 22
          }
        },
        loc: {
          start: {
            line: 17,
            column: 40
          },
          end: {
            line: 33,
            column: 1
          }
        },
        line: 17
      },
      '3': {
        name: '(anonymous_3)',
        decl: {
          start: {
            line: 18,
            column: 19
          },
          end: {
            line: 18,
            column: 20
          }
        },
        loc: {
          start: {
            line: 18,
            column: 27
          },
          end: {
            line: 32,
            column: 3
          }
        },
        line: 18
      },
      '4': {
        name: 'formatAttributes',
        decl: {
          start: {
            line: 35,
            column: 16
          },
          end: {
            line: 35,
            column: 32
          }
        },
        loc: {
          start: {
            line: 35,
            column: 46
          },
          end: {
            line: 44,
            column: 1
          }
        },
        line: 35
      },
      '5': {
        name: '(anonymous_5)',
        decl: {
          start: {
            line: 36,
            column: 24
          },
          end: {
            line: 36,
            column: 25
          }
        },
        loc: {
          start: {
            line: 36,
            column: 37
          },
          end: {
            line: 43,
            column: 3
          }
        },
        line: 36
      }
    },
    branchMap: {
      '0': {
        loc: {
          start: {
            line: 3,
            column: 2
          },
          end: {
            line: 3,
            column: 30
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 3,
            column: 2
          },
          end: {
            line: 3,
            column: 30
          }
        }, {
          start: {
            line: 3,
            column: 2
          },
          end: {
            line: 3,
            column: 30
          }
        }],
        line: 3
      },
      '1': {
        loc: {
          start: {
            line: 10,
            column: 23
          },
          end: {
            line: 10,
            column: 49
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 10,
            column: 23
          },
          end: {
            line: 10,
            column: 34
          }
        }, {
          start: {
            line: 10,
            column: 38
          },
          end: {
            line: 10,
            column: 49
          }
        }],
        line: 10
      },
      '2': {
        loc: {
          start: {
            line: 11,
            column: 2
          },
          end: {
            line: 13,
            column: 3
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 11,
            column: 2
          },
          end: {
            line: 13,
            column: 3
          }
        }, {
          start: {
            line: 11,
            column: 2
          },
          end: {
            line: 13,
            column: 3
          }
        }],
        line: 11
      },
      '3': {
        loc: {
          start: {
            line: 11,
            column: 6
          },
          end: {
            line: 11,
            column: 45
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 11,
            column: 6
          },
          end: {
            line: 11,
            column: 18
          }
        }, {
          start: {
            line: 11,
            column: 22
          },
          end: {
            line: 11,
            column: 45
          }
        }],
        line: 11
      },
      '4': {
        loc: {
          start: {
            line: 20,
            column: 23
          },
          end: {
            line: 27,
            column: 39
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 21,
            column: 8
          },
          end: {
            line: 26,
            column: 7
          }
        }, {
          start: {
            line: 27,
            column: 8
          },
          end: {
            line: 27,
            column: 39
          }
        }],
        line: 20
      },
      '5': {
        loc: {
          start: {
            line: 28,
            column: 4
          },
          end: {
            line: 30,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 28,
            column: 4
          },
          end: {
            line: 30,
            column: 5
          }
        }, {
          start: {
            line: 28,
            column: 4
          },
          end: {
            line: 30,
            column: 5
          }
        }],
        line: 28
      },
      '6': {
        loc: {
          start: {
            line: 39,
            column: 18
          },
          end: {
            line: 41,
            column: 12
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 40,
            column: 8
          },
          end: {
            line: 40,
            column: 25
          }
        }, {
          start: {
            line: 41,
            column: 8
          },
          end: {
            line: 41,
            column: 12
          }
        }],
        line: 39
      }
    },
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0,
      '6': 0,
      '7': 0,
      '8': 0,
      '9': 0,
      '10': 0,
      '11': 0,
      '12': 0,
      '13': 0,
      '14': 0,
      '15': 0,
      '16': 0,
      '17': 0,
      '18': 0,
      '19': 0,
      '20': 0
    },
    f: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0
    },
    b: {
      '0': [0, 0],
      '1': [0, 0],
      '2': [0, 0],
      '3': [0, 0],
      '4': [0, 0],
      '5': [0, 0],
      '6': [0, 0]
    },
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.splitHead = splitHead;
exports.unquote = unquote;
exports.format = format;
exports.formatAttributes = formatAttributes;
function splitHead(str, sep) {
  cov_1xnzystgba.f[0]++;

  var idx = (cov_1xnzystgba.s[0]++, str.indexOf(sep));
  cov_1xnzystgba.s[1]++;
  if (idx === -1) {
      cov_1xnzystgba.b[0][0]++;
      cov_1xnzystgba.s[2]++;
      return [str];
    } else {
    cov_1xnzystgba.b[0][1]++;
  }cov_1xnzystgba.s[3]++;
  return [str.slice(0, idx), str.slice(idx + sep.length)];
}

function unquote(str) {
  cov_1xnzystgba.f[1]++;

  var car = (cov_1xnzystgba.s[4]++, str.charAt(0));
  var end = (cov_1xnzystgba.s[5]++, str.length - 1);
  var isQuoteStart = (cov_1xnzystgba.s[6]++, (cov_1xnzystgba.b[1][0]++, car === '"') || (cov_1xnzystgba.b[1][1]++, car === "'"));
  cov_1xnzystgba.s[7]++;
  if ((cov_1xnzystgba.b[3][0]++, isQuoteStart) && (cov_1xnzystgba.b[3][1]++, car === str.charAt(end))) {
    cov_1xnzystgba.b[2][0]++;
    cov_1xnzystgba.s[8]++;

    return str.slice(1, end);
  } else {
    cov_1xnzystgba.b[2][1]++;
  }
  cov_1xnzystgba.s[9]++;
  return str;
}

function format(nodes, options) {
  cov_1xnzystgba.f[2]++;
  cov_1xnzystgba.s[10]++;

  return nodes.map(function (node) {
    cov_1xnzystgba.f[3]++;

    var type = (cov_1xnzystgba.s[11]++, node.type);
    var outputNode = (cov_1xnzystgba.s[12]++, type === 'element' ? (cov_1xnzystgba.b[4][0]++, {
      type: type,
      tagName: node.tagName.toLowerCase(),
      attributes: formatAttributes(node.attributes),
      children: format(node.children, options)
    }) : (cov_1xnzystgba.b[4][1]++, { type: type, content: node.content }));
    cov_1xnzystgba.s[13]++;
    if (options.includePositions) {
      cov_1xnzystgba.b[5][0]++;
      cov_1xnzystgba.s[14]++;

      outputNode.position = node.position;
    } else {
      cov_1xnzystgba.b[5][1]++;
    }
    cov_1xnzystgba.s[15]++;
    return outputNode;
  });
}

function formatAttributes(attributes) {
  cov_1xnzystgba.f[4]++;
  cov_1xnzystgba.s[16]++;

  return attributes.map(function (attribute) {
    cov_1xnzystgba.f[5]++;

    var parts = (cov_1xnzystgba.s[17]++, splitHead(attribute.trim(), '='));
    var key = (cov_1xnzystgba.s[18]++, parts[0]);
    var value = (cov_1xnzystgba.s[19]++, typeof parts[1] === 'string' ? (cov_1xnzystgba.b[6][0]++, unquote(parts[1])) : (cov_1xnzystgba.b[6][1]++, null));
    cov_1xnzystgba.s[20]++;
    return { key: key, value: value };
  });
}

},{}],3:[function(require,module,exports){
'use strict';

var cov_1drn7jthmy = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/index.js',
      hash = 'a91ca68b6320b199fa63e4cbd37dce6857e0c43d',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/index.js',
    statementMap: {
      '0': {
        start: {
          line: 12,
          column: 29
        },
        end: {
          line: 18,
          column: 1
        }
      },
      '1': {
        start: {
          line: 21,
          column: 17
        },
        end: {
          line: 21,
          column: 36
        }
      },
      '2': {
        start: {
          line: 22,
          column: 16
        },
        end: {
          line: 22,
          column: 39
        }
      },
      '3': {
        start: {
          line: 23,
          column: 2
        },
        end: {
          line: 23,
          column: 31
        }
      },
      '4': {
        start: {
          line: 27,
          column: 2
        },
        end: {
          line: 27,
          column: 29
        }
      }
    },
    fnMap: {
      '0': {
        name: 'parse',
        decl: {
          start: {
            line: 20,
            column: 16
          },
          end: {
            line: 20,
            column: 21
          }
        },
        loc: {
          start: {
            line: 20,
            column: 53
          },
          end: {
            line: 24,
            column: 1
          }
        },
        line: 20
      },
      '1': {
        name: 'stringify',
        decl: {
          start: {
            line: 26,
            column: 16
          },
          end: {
            line: 26,
            column: 25
          }
        },
        loc: {
          start: {
            line: 26,
            column: 57
          },
          end: {
            line: 28,
            column: 1
          }
        },
        line: 26
      }
    },
    branchMap: {
      '0': {
        loc: {
          start: {
            line: 20,
            column: 28
          },
          end: {
            line: 20,
            column: 51
          }
        },
        type: 'default-arg',
        locations: [{
          start: {
            line: 20,
            column: 38
          },
          end: {
            line: 20,
            column: 51
          }
        }],
        line: 20
      },
      '1': {
        loc: {
          start: {
            line: 26,
            column: 32
          },
          end: {
            line: 26,
            column: 55
          }
        },
        type: 'default-arg',
        locations: [{
          start: {
            line: 26,
            column: 42
          },
          end: {
            line: 26,
            column: 55
          }
        }],
        line: 26
      }
    },
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0
    },
    f: {
      '0': 0,
      '1': 0
    },
    b: {
      '0': [0],
      '1': [0]
    },
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.parseDefaults = undefined;
exports.parse = parse;
exports.stringify = stringify;

var _lexer = require('./lexer');

var _lexer2 = _interopRequireDefault(_lexer);

var _parser = require('./parser');

var _parser2 = _interopRequireDefault(_parser);

var _format = require('./format');

var _stringify = require('./stringify');

var _tags = require('./tags');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var parseDefaults = exports.parseDefaults = (cov_1drn7jthmy.s[0]++, {
  voidTags: _tags.voidTags,
  closingTags: _tags.closingTags,
  childlessTags: _tags.childlessTags,
  closingTagAncestorBreakers: _tags.closingTagAncestorBreakers,
  includePositions: false
});

function parse(str) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : (cov_1drn7jthmy.b[0][0]++, parseDefaults);
  cov_1drn7jthmy.f[0]++;

  var tokens = (cov_1drn7jthmy.s[1]++, (0, _lexer2.default)(str, options));
  var nodes = (cov_1drn7jthmy.s[2]++, (0, _parser2.default)(tokens, options));
  cov_1drn7jthmy.s[3]++;
  return (0, _format.format)(nodes, options);
}

function stringify(ast) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : (cov_1drn7jthmy.b[1][0]++, parseDefaults);
  cov_1drn7jthmy.f[1]++;
  cov_1drn7jthmy.s[4]++;

  return (0, _stringify.toHTML)(ast, options);
}

},{"./format":2,"./lexer":4,"./parser":5,"./stringify":6,"./tags":7}],4:[function(require,module,exports){
'use strict';

var cov_1mknr9mehe = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/lexer.js',
      hash = '99f1269b85a36e02e6fcfa2eb5c9423a8a428848',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/lexer.js',
    statementMap: {
      '0': {
        start: {
          line: 9,
          column: 16
        },
        end: {
          line: 9,
          column: 30
        }
      },
      '1': {
        start: {
          line: 10,
          column: 14
        },
        end: {
          line: 10,
          column: 42
        }
      },
      '2': {
        start: {
          line: 11,
          column: 2
        },
        end: {
          line: 19,
          column: 3
        }
      },
      '3': {
        start: {
          line: 12,
          column: 17
        },
        end: {
          line: 12,
          column: 30
        }
      },
      '4': {
        start: {
          line: 13,
          column: 4
        },
        end: {
          line: 18,
          column: 5
        }
      },
      '5': {
        start: {
          line: 14,
          column: 6
        },
        end: {
          line: 14,
          column: 21
        }
      },
      '6': {
        start: {
          line: 15,
          column: 6
        },
        end: {
          line: 15,
          column: 25
        }
      },
      '7': {
        start: {
          line: 17,
          column: 6
        },
        end: {
          line: 17,
          column: 23
        }
      },
      '8': {
        start: {
          line: 23,
          column: 14
        },
        end: {
          line: 23,
          column: 34
        }
      },
      '9': {
        start: {
          line: 24,
          column: 2
        },
        end: {
          line: 24,
          column: 41
        }
      },
      '10': {
        start: {
          line: 28,
          column: 2
        },
        end: {
          line: 32,
          column: 3
        }
      },
      '11': {
        start: {
          line: 36,
          column: 2
        },
        end: {
          line: 40,
          column: 3
        }
      },
      '12': {
        start: {
          line: 44,
          column: 16
        },
        end: {
          line: 49,
          column: 3
        }
      },
      '13': {
        start: {
          line: 50,
          column: 2
        },
        end: {
          line: 50,
          column: 12
        }
      },
      '14': {
        start: {
          line: 51,
          column: 2
        },
        end: {
          line: 51,
          column: 21
        }
      },
      '15': {
        start: {
          line: 55,
          column: 42
        },
        end: {
          line: 55,
          column: 47
        }
      },
      '16': {
        start: {
          line: 56,
          column: 14
        },
        end: {
          line: 56,
          column: 24
        }
      },
      '17': {
        start: {
          line: 57,
          column: 2
        },
        end: {
          line: 72,
          column: 3
        }
      },
      '18': {
        start: {
          line: 58,
          column: 18
        },
        end: {
          line: 58,
          column: 38
        }
      },
      '19': {
        start: {
          line: 59,
          column: 4
        },
        end: {
          line: 59,
          column: 18
        }
      },
      '20': {
        start: {
          line: 60,
          column: 4
        },
        end: {
          line: 71,
          column: 5
        }
      },
      '21': {
        start: {
          line: 61,
          column: 24
        },
        end: {
          line: 61,
          column: 57
        }
      },
      '22': {
        start: {
          line: 62,
          column: 6
        },
        end: {
          line: 70,
          column: 7
        }
      },
      '23': {
        start: {
          line: 63,
          column: 8
        },
        end: {
          line: 63,
          column: 25
        }
      },
      '24': {
        start: {
          line: 65,
          column: 24
        },
        end: {
          line: 65,
          column: 37
        }
      },
      '25': {
        start: {
          line: 66,
          column: 24
        },
        end: {
          line: 66,
          column: 45
        }
      },
      '26': {
        start: {
          line: 67,
          column: 8
        },
        end: {
          line: 69,
          column: 9
        }
      },
      '27': {
        start: {
          line: 68,
          column: 10
        },
        end: {
          line: 68,
          column: 36
        }
      },
      '28': {
        start: {
          line: 75,
          column: 21
        },
        end: {
          line: 75,
          column: 34
        }
      },
      '29': {
        start: {
          line: 77,
          column: 2
        },
        end: {
          line: 87,
          column: 3
        }
      },
      '30': {
        start: {
          line: 78,
          column: 20
        },
        end: {
          line: 78,
          column: 43
        }
      },
      '31': {
        start: {
          line: 79,
          column: 4
        },
        end: {
          line: 81,
          column: 5
        }
      },
      '32': {
        start: {
          line: 80,
          column: 6
        },
        end: {
          line: 80,
          column: 20
        }
      },
      '33': {
        start: {
          line: 82,
          column: 17
        },
        end: {
          line: 82,
          column: 40
        }
      },
      '34': {
        start: {
          line: 83,
          column: 4
        },
        end: {
          line: 85,
          column: 5
        }
      },
      '35': {
        start: {
          line: 84,
          column: 6
        },
        end: {
          line: 84,
          column: 20
        }
      },
      '36': {
        start: {
          line: 86,
          column: 4
        },
        end: {
          line: 86,
          column: 23
        }
      },
      '37': {
        start: {
          line: 91,
          column: 15
        },
        end: {
          line: 91,
          column: 21
        }
      },
      '38': {
        start: {
          line: 92,
          column: 26
        },
        end: {
          line: 92,
          column: 31
        }
      },
      '39': {
        start: {
          line: 93,
          column: 16
        },
        end: {
          line: 93,
          column: 48
        }
      },
      '40': {
        start: {
          line: 94,
          column: 2
        },
        end: {
          line: 94,
          column: 40
        }
      },
      '41': {
        start: {
          line: 94,
          column: 34
        },
        end: {
          line: 94,
          column: 40
        }
      },
      '42': {
        start: {
          line: 95,
          column: 2
        },
        end: {
          line: 97,
          column: 3
        }
      },
      '43': {
        start: {
          line: 96,
          column: 4
        },
        end: {
          line: 96,
          column: 24
        }
      },
      '44': {
        start: {
          line: 99,
          column: 16
        },
        end: {
          line: 99,
          column: 38
        }
      },
      '45': {
        start: {
          line: 100,
          column: 18
        },
        end: {
          line: 100,
          column: 52
        }
      },
      '46': {
        start: {
          line: 101,
          column: 2
        },
        end: {
          line: 101,
          column: 38
        }
      },
      '47': {
        start: {
          line: 102,
          column: 14
        },
        end: {
          line: 102,
          column: 36
        }
      },
      '48': {
        start: {
          line: 103,
          column: 2
        },
        end: {
          line: 103,
          column: 60
        }
      },
      '49': {
        start: {
          line: 107,
          column: 26
        },
        end: {
          line: 107,
          column: 31
        }
      },
      '50': {
        start: {
          line: 108,
          column: 16
        },
        end: {
          line: 108,
          column: 38
        }
      },
      '51': {
        start: {
          line: 109,
          column: 2
        },
        end: {
          line: 109,
          column: 32
        }
      },
      '52': {
        start: {
          line: 110,
          column: 19
        },
        end: {
          line: 110,
          column: 53
        }
      },
      '53': {
        start: {
          line: 111,
          column: 19
        },
        end: {
          line: 111,
          column: 33
        }
      },
      '54': {
        start: {
          line: 112,
          column: 2
        },
        end: {
          line: 114,
          column: 3
        }
      },
      '55': {
        start: {
          line: 113,
          column: 4
        },
        end: {
          line: 113,
          column: 40
        }
      },
      '56': {
        start: {
          line: 116,
          column: 18
        },
        end: {
          line: 116,
          column: 55
        }
      },
      '57': {
        start: {
          line: 117,
          column: 2
        },
        end: {
          line: 117,
          column: 41
        }
      },
      '58': {
        start: {
          line: 118,
          column: 2
        },
        end: {
          line: 125,
          column: 4
        }
      },
      '59': {
        start: {
          line: 129,
          column: 26
        },
        end: {
          line: 129,
          column: 31
        }
      },
      '60': {
        start: {
          line: 131,
          column: 23
        },
        end: {
          line: 131,
          column: 53
        }
      },
      '61': {
        start: {
          line: 132,
          column: 18
        },
        end: {
          line: 132,
          column: 36
        }
      },
      '62': {
        start: {
          line: 133,
          column: 18
        },
        end: {
          line: 133,
          column: 40
        }
      },
      '63': {
        start: {
          line: 134,
          column: 4
        },
        end: {
          line: 134,
          column: 46
        }
      },
      '64': {
        start: {
          line: 135,
          column: 4
        },
        end: {
          line: 135,
          column: 68
        }
      },
      '65': {
        start: {
          line: 137,
          column: 18
        },
        end: {
          line: 137,
          column: 35
        }
      },
      '66': {
        start: {
          line: 138,
          column: 2
        },
        end: {
          line: 138,
          column: 25
        }
      },
      '67': {
        start: {
          line: 140,
          column: 22
        },
        end: {
          line: 140,
          column: 48
        }
      },
      '68': {
        start: {
          line: 141,
          column: 18
        },
        end: {
          line: 141,
          column: 35
        }
      },
      '69': {
        start: {
          line: 142,
          column: 4
        },
        end: {
          line: 142,
          column: 46
        }
      },
      '70': {
        start: {
          line: 143,
          column: 16
        },
        end: {
          line: 143,
          column: 38
        }
      },
      '71': {
        start: {
          line: 144,
          column: 4
        },
        end: {
          line: 144,
          column: 64
        }
      },
      '72': {
        start: {
          line: 146,
          column: 2
        },
        end: {
          line: 146,
          column: 16
        }
      },
      '73': {
        start: {
          line: 150,
          column: 19
        },
        end: {
          line: 150,
          column: 23
        }
      },
      '74': {
        start: {
          line: 152,
          column: 2
        },
        end: {
          line: 152,
          column: 30
        }
      },
      '75': {
        start: {
          line: 156,
          column: 26
        },
        end: {
          line: 156,
          column: 31
        }
      },
      '76': {
        start: {
          line: 157,
          column: 14
        },
        end: {
          line: 157,
          column: 24
        }
      },
      '77': {
        start: {
          line: 158,
          column: 14
        },
        end: {
          line: 158,
          column: 28
        }
      },
      '78': {
        start: {
          line: 159,
          column: 2
        },
        end: {
          line: 164,
          column: 3
        }
      },
      '79': {
        start: {
          line: 160,
          column: 17
        },
        end: {
          line: 160,
          column: 34
        }
      },
      '80': {
        start: {
          line: 161,
          column: 22
        },
        end: {
          line: 161,
          column: 79
        }
      },
      '81': {
        start: {
          line: 162,
          column: 4
        },
        end: {
          line: 162,
          column: 24
        }
      },
      '82': {
        start: {
          line: 162,
          column: 19
        },
        end: {
          line: 162,
          column: 24
        }
      },
      '83': {
        start: {
          line: 163,
          column: 4
        },
        end: {
          line: 163,
          column: 11
        }
      },
      '84': {
        start: {
          line: 166,
          column: 12
        },
        end: {
          line: 166,
          column: 21
        }
      },
      '85': {
        start: {
          line: 167,
          column: 2
        },
        end: {
          line: 172,
          column: 3
        }
      },
      '86': {
        start: {
          line: 168,
          column: 17
        },
        end: {
          line: 168,
          column: 32
        }
      },
      '87': {
        start: {
          line: 169,
          column: 22
        },
        end: {
          line: 169,
          column: 79
        }
      },
      '88': {
        start: {
          line: 170,
          column: 4
        },
        end: {
          line: 170,
          column: 25
        }
      },
      '89': {
        start: {
          line: 170,
          column: 20
        },
        end: {
          line: 170,
          column: 25
        }
      },
      '90': {
        start: {
          line: 171,
          column: 4
        },
        end: {
          line: 171,
          column: 9
        }
      },
      '91': {
        start: {
          line: 174,
          column: 2
        },
        end: {
          line: 174,
          column: 34
        }
      },
      '92': {
        start: {
          line: 175,
          column: 18
        },
        end: {
          line: 175,
          column: 39
        }
      },
      '93': {
        start: {
          line: 176,
          column: 2
        },
        end: {
          line: 179,
          column: 4
        }
      },
      '94': {
        start: {
          line: 180,
          column: 2
        },
        end: {
          line: 180,
          column: 16
        }
      },
      '95': {
        start: {
          line: 184,
          column: 34
        },
        end: {
          line: 184,
          column: 39
        }
      },
      '96': {
        start: {
          line: 185,
          column: 15
        },
        end: {
          line: 185,
          column: 29
        }
      },
      '97': {
        start: {
          line: 186,
          column: 14
        },
        end: {
          line: 186,
          column: 18
        }
      },
      '98': {
        start: {
          line: 187,
          column: 18
        },
        end: {
          line: 187,
          column: 24
        }
      },
      '99': {
        start: {
          line: 188,
          column: 16
        },
        end: {
          line: 188,
          column: 18
        }
      },
      '100': {
        start: {
          line: 189,
          column: 14
        },
        end: {
          line: 189,
          column: 24
        }
      },
      '101': {
        start: {
          line: 190,
          column: 2
        },
        end: {
          line: 227,
          column: 3
        }
      },
      '102': {
        start: {
          line: 191,
          column: 17
        },
        end: {
          line: 191,
          column: 35
        }
      },
      '103': {
        start: {
          line: 192,
          column: 4
        },
        end: {
          line: 199,
          column: 5
        }
      },
      '104': {
        start: {
          line: 193,
          column: 25
        },
        end: {
          line: 193,
          column: 39
        }
      },
      '105': {
        start: {
          line: 194,
          column: 6
        },
        end: {
          line: 196,
          column: 7
        }
      },
      '106': {
        start: {
          line: 195,
          column: 8
        },
        end: {
          line: 195,
          column: 20
        }
      },
      '107': {
        start: {
          line: 197,
          column: 6
        },
        end: {
          line: 197,
          column: 14
        }
      },
      '108': {
        start: {
          line: 198,
          column: 6
        },
        end: {
          line: 198,
          column: 14
        }
      },
      '109': {
        start: {
          line: 201,
          column: 21
        },
        end: {
          line: 201,
          column: 49
        }
      },
      '110': {
        start: {
          line: 202,
          column: 4
        },
        end: {
          line: 207,
          column: 5
        }
      },
      '111': {
        start: {
          line: 203,
          column: 6
        },
        end: {
          line: 205,
          column: 7
        }
      },
      '112': {
        start: {
          line: 204,
          column: 8
        },
        end: {
          line: 204,
          column: 48
        }
      },
      '113': {
        start: {
          line: 206,
          column: 6
        },
        end: {
          line: 206,
          column: 11
        }
      },
      '114': {
        start: {
          line: 209,
          column: 22
        },
        end: {
          line: 209,
          column: 44
        }
      },
      '115': {
        start: {
          line: 210,
          column: 4
        },
        end: {
          line: 217,
          column: 5
        }
      },
      '116': {
        start: {
          line: 211,
          column: 6
        },
        end: {
          line: 213,
          column: 7
        }
      },
      '117': {
        start: {
          line: 212,
          column: 8
        },
        end: {
          line: 212,
          column: 48
        }
      },
      '118': {
        start: {
          line: 214,
          column: 6
        },
        end: {
          line: 214,
          column: 28
        }
      },
      '119': {
        start: {
          line: 215,
          column: 6
        },
        end: {
          line: 215,
          column: 14
        }
      },
      '120': {
        start: {
          line: 216,
          column: 6
        },
        end: {
          line: 216,
          column: 14
        }
      },
      '121': {
        start: {
          line: 219,
          column: 25
        },
        end: {
          line: 219,
          column: 54
        }
      },
      '122': {
        start: {
          line: 220,
          column: 4
        },
        end: {
          line: 224,
          column: 5
        }
      },
      '123': {
        start: {
          line: 221,
          column: 6
        },
        end: {
          line: 221,
          column: 18
        }
      },
      '124': {
        start: {
          line: 222,
          column: 6
        },
        end: {
          line: 222,
          column: 14
        }
      },
      '125': {
        start: {
          line: 223,
          column: 6
        },
        end: {
          line: 223,
          column: 14
        }
      },
      '126': {
        start: {
          line: 226,
          column: 4
        },
        end: {
          line: 226,
          column: 12
        }
      },
      '127': {
        start: {
          line: 228,
          column: 2
        },
        end: {
          line: 228,
          column: 37
        }
      },
      '128': {
        start: {
          line: 230,
          column: 15
        },
        end: {
          line: 230,
          column: 27
        }
      },
      '129': {
        start: {
          line: 231,
          column: 15
        },
        end: {
          line: 231,
          column: 26
        }
      },
      '130': {
        start: {
          line: 232,
          column: 2
        },
        end: {
          line: 269,
          column: 3
        }
      },
      '131': {
        start: {
          line: 233,
          column: 17
        },
        end: {
          line: 233,
          column: 25
        }
      },
      '132': {
        start: {
          line: 234,
          column: 22
        },
        end: {
          line: 234,
          column: 46
        }
      },
      '133': {
        start: {
          line: 235,
          column: 4
        },
        end: {
          line: 253,
          column: 5
        }
      },
      '134': {
        start: {
          line: 236,
          column: 25
        },
        end: {
          line: 236,
          column: 37
        }
      },
      '135': {
        start: {
          line: 237,
          column: 6
        },
        end: {
          line: 252,
          column: 7
        }
      },
      '136': {
        start: {
          line: 238,
          column: 8
        },
        end: {
          line: 243,
          column: 9
        }
      },
      '137': {
        start: {
          line: 239,
          column: 26
        },
        end: {
          line: 239,
          column: 43
        }
      },
      '138': {
        start: {
          line: 240,
          column: 10
        },
        end: {
          line: 240,
          column: 47
        }
      },
      '139': {
        start: {
          line: 241,
          column: 10
        },
        end: {
          line: 241,
          column: 16
        }
      },
      '140': {
        start: {
          line: 242,
          column: 10
        },
        end: {
          line: 242,
          column: 18
        }
      },
      '141': {
        start: {
          line: 244,
          column: 26
        },
        end: {
          line: 244,
          column: 38
        }
      },
      '142': {
        start: {
          line: 245,
          column: 8
        },
        end: {
          line: 245,
          column: 14
        }
      },
      '143': {
        start: {
          line: 246,
          column: 8
        },
        end: {
          line: 251,
          column: 9
        }
      },
      '144': {
        start: {
          line: 247,
          column: 26
        },
        end: {
          line: 247,
          column: 48
        }
      },
      '145': {
        start: {
          line: 248,
          column: 10
        },
        end: {
          line: 248,
          column: 47
        }
      },
      '146': {
        start: {
          line: 249,
          column: 10
        },
        end: {
          line: 249,
          column: 16
        }
      },
      '147': {
        start: {
          line: 250,
          column: 10
        },
        end: {
          line: 250,
          column: 18
        }
      },
      '148': {
        start: {
          line: 254,
          column: 4
        },
        end: {
          line: 266,
          column: 5
        }
      },
      '149': {
        start: {
          line: 255,
          column: 25
        },
        end: {
          line: 255,
          column: 37
        }
      },
      '150': {
        start: {
          line: 256,
          column: 6
        },
        end: {
          line: 261,
          column: 7
        }
      },
      '151': {
        start: {
          line: 257,
          column: 24
        },
        end: {
          line: 257,
          column: 41
        }
      },
      '152': {
        start: {
          line: 258,
          column: 8
        },
        end: {
          line: 258,
          column: 45
        }
      },
      '153': {
        start: {
          line: 259,
          column: 8
        },
        end: {
          line: 259,
          column: 14
        }
      },
      '154': {
        start: {
          line: 260,
          column: 8
        },
        end: {
          line: 260,
          column: 16
        }
      },
      '155': {
        start: {
          line: 263,
          column: 22
        },
        end: {
          line: 263,
          column: 39
        }
      },
      '156': {
        start: {
          line: 264,
          column: 6
        },
        end: {
          line: 264,
          column: 43
        }
      },
      '157': {
        start: {
          line: 265,
          column: 6
        },
        end: {
          line: 265,
          column: 14
        }
      },
      '158': {
        start: {
          line: 268,
          column: 4
        },
        end: {
          line: 268,
          column: 38
        }
      },
      '159': {
        start: {
          line: 272,
          column: 13
        },
        end: {
          line: 272,
          column: 20
        }
      },
      '160': {
        start: {
          line: 275,
          column: 34
        },
        end: {
          line: 275,
          column: 39
        }
      },
      '161': {
        start: {
          line: 276,
          column: 22
        },
        end: {
          line: 276,
          column: 43
        }
      },
      '162': {
        start: {
          line: 277,
          column: 14
        },
        end: {
          line: 277,
          column: 24
        }
      },
      '163': {
        start: {
          line: 278,
          column: 14
        },
        end: {
          line: 278,
          column: 28
        }
      },
      '164': {
        start: {
          line: 279,
          column: 2
        },
        end: {
          line: 311,
          column: 3
        }
      },
      '165': {
        start: {
          line: 280,
          column: 20
        },
        end: {
          line: 280,
          column: 44
        }
      },
      '166': {
        start: {
          line: 281,
          column: 4
        },
        end: {
          line: 284,
          column: 5
        }
      },
      '167': {
        start: {
          line: 282,
          column: 6
        },
        end: {
          line: 282,
          column: 20
        }
      },
      '168': {
        start: {
          line: 283,
          column: 6
        },
        end: {
          line: 283,
          column: 11
        }
      },
      '169': {
        start: {
          line: 286,
          column: 29
        },
        end: {
          line: 286,
          column: 51
        }
      },
      '170': {
        start: {
          line: 287,
          column: 4
        },
        end: {
          line: 287,
          column: 48
        }
      },
      '171': {
        start: {
          line: 288,
          column: 21
        },
        end: {
          line: 288,
          column: 66
        }
      },
      '172': {
        start: {
          line: 289,
          column: 17
        },
        end: {
          line: 289,
          column: 33
        }
      },
      '173': {
        start: {
          line: 290,
          column: 4
        },
        end: {
          line: 293,
          column: 5
        }
      },
      '174': {
        start: {
          line: 291,
          column: 6
        },
        end: {
          line: 291,
          column: 37
        }
      },
      '175': {
        start: {
          line: 292,
          column: 6
        },
        end: {
          line: 292,
          column: 14
        }
      },
      '176': {
        start: {
          line: 295,
          column: 4
        },
        end: {
          line: 306,
          column: 5
        }
      },
      '177': {
        start: {
          line: 296,
          column: 24
        },
        end: {
          line: 296,
          column: 46
        }
      },
      '178': {
        start: {
          line: 297,
          column: 6
        },
        end: {
          line: 297,
          column: 42
        }
      },
      '179': {
        start: {
          line: 298,
          column: 6
        },
        end: {
          line: 305,
          column: 8
        }
      },
      '180': {
        start: {
          line: 308,
          column: 4
        },
        end: {
          line: 308,
          column: 39
        }
      },
      '181': {
        start: {
          line: 309,
          column: 4
        },
        end: {
          line: 309,
          column: 56
        }
      },
      '182': {
        start: {
          line: 310,
          column: 4
        },
        end: {
          line: 310,
          column: 9
        }
      }
    },
    fnMap: {
      '0': {
        name: 'feedPosition',
        decl: {
          start: {
            line: 8,
            column: 16
          },
          end: {
            line: 8,
            column: 28
          }
        },
        loc: {
          start: {
            line: 8,
            column: 50
          },
          end: {
            line: 20,
            column: 1
          }
        },
        line: 8
      },
      '1': {
        name: 'jumpPosition',
        decl: {
          start: {
            line: 22,
            column: 16
          },
          end: {
            line: 22,
            column: 28
          }
        },
        loc: {
          start: {
            line: 22,
            column: 50
          },
          end: {
            line: 25,
            column: 1
          }
        },
        line: 22
      },
      '2': {
        name: 'makeInitialPosition',
        decl: {
          start: {
            line: 27,
            column: 16
          },
          end: {
            line: 27,
            column: 35
          }
        },
        loc: {
          start: {
            line: 27,
            column: 39
          },
          end: {
            line: 33,
            column: 1
          }
        },
        line: 27
      },
      '3': {
        name: 'copyPosition',
        decl: {
          start: {
            line: 35,
            column: 16
          },
          end: {
            line: 35,
            column: 28
          }
        },
        loc: {
          start: {
            line: 35,
            column: 40
          },
          end: {
            line: 41,
            column: 1
          }
        },
        line: 35
      },
      '4': {
        name: 'lexer',
        decl: {
          start: {
            line: 43,
            column: 24
          },
          end: {
            line: 43,
            column: 29
          }
        },
        loc: {
          start: {
            line: 43,
            column: 45
          },
          end: {
            line: 52,
            column: 1
          }
        },
        line: 43
      },
      '5': {
        name: 'lex',
        decl: {
          start: {
            line: 54,
            column: 16
          },
          end: {
            line: 54,
            column: 19
          }
        },
        loc: {
          start: {
            line: 54,
            column: 28
          },
          end: {
            line: 73,
            column: 1
          }
        },
        line: 54
      },
      '6': {
        name: 'findTextEnd',
        decl: {
          start: {
            line: 76,
            column: 16
          },
          end: {
            line: 76,
            column: 27
          }
        },
        loc: {
          start: {
            line: 76,
            column: 41
          },
          end: {
            line: 88,
            column: 1
          }
        },
        line: 76
      },
      '7': {
        name: 'lexText',
        decl: {
          start: {
            line: 90,
            column: 16
          },
          end: {
            line: 90,
            column: 23
          }
        },
        loc: {
          start: {
            line: 90,
            column: 32
          },
          end: {
            line: 104,
            column: 1
          }
        },
        line: 90
      },
      '8': {
        name: 'lexComment',
        decl: {
          start: {
            line: 106,
            column: 16
          },
          end: {
            line: 106,
            column: 26
          }
        },
        loc: {
          start: {
            line: 106,
            column: 35
          },
          end: {
            line: 126,
            column: 1
          }
        },
        line: 106
      },
      '9': {
        name: 'lexTag',
        decl: {
          start: {
            line: 128,
            column: 16
          },
          end: {
            line: 128,
            column: 22
          }
        },
        loc: {
          start: {
            line: 128,
            column: 31
          },
          end: {
            line: 147,
            column: 1
          }
        },
        line: 128
      },
      '10': {
        name: 'isWhitespaceChar',
        decl: {
          start: {
            line: 151,
            column: 16
          },
          end: {
            line: 151,
            column: 32
          }
        },
        loc: {
          start: {
            line: 151,
            column: 40
          },
          end: {
            line: 153,
            column: 1
          }
        },
        line: 151
      },
      '11': {
        name: 'lexTagName',
        decl: {
          start: {
            line: 155,
            column: 16
          },
          end: {
            line: 155,
            column: 26
          }
        },
        loc: {
          start: {
            line: 155,
            column: 35
          },
          end: {
            line: 181,
            column: 1
          }
        },
        line: 155
      },
      '12': {
        name: 'lexTagAttributes',
        decl: {
          start: {
            line: 183,
            column: 16
          },
          end: {
            line: 183,
            column: 32
          }
        },
        loc: {
          start: {
            line: 183,
            column: 41
          },
          end: {
            line: 270,
            column: 1
          }
        },
        line: 183
      },
      '13': {
        name: 'lexSkipTag',
        decl: {
          start: {
            line: 274,
            column: 16
          },
          end: {
            line: 274,
            column: 26
          }
        },
        loc: {
          start: {
            line: 274,
            column: 44
          },
          end: {
            line: 312,
            column: 1
          }
        },
        line: 274
      }
    },
    branchMap: {
      '0': {
        loc: {
          start: {
            line: 13,
            column: 4
          },
          end: {
            line: 18,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 13,
            column: 4
          },
          end: {
            line: 18,
            column: 5
          }
        }, {
          start: {
            line: 13,
            column: 4
          },
          end: {
            line: 18,
            column: 5
          }
        }],
        line: 13
      },
      '1': {
        loc: {
          start: {
            line: 60,
            column: 4
          },
          end: {
            line: 71,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 60,
            column: 4
          },
          end: {
            line: 71,
            column: 5
          }
        }, {
          start: {
            line: 60,
            column: 4
          },
          end: {
            line: 71,
            column: 5
          }
        }],
        line: 60
      },
      '2': {
        loc: {
          start: {
            line: 62,
            column: 6
          },
          end: {
            line: 70,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 62,
            column: 6
          },
          end: {
            line: 70,
            column: 7
          }
        }, {
          start: {
            line: 62,
            column: 6
          },
          end: {
            line: 70,
            column: 7
          }
        }],
        line: 62
      },
      '3': {
        loc: {
          start: {
            line: 67,
            column: 8
          },
          end: {
            line: 69,
            column: 9
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 67,
            column: 8
          },
          end: {
            line: 69,
            column: 9
          }
        }, {
          start: {
            line: 67,
            column: 8
          },
          end: {
            line: 69,
            column: 9
          }
        }],
        line: 67
      },
      '4': {
        loc: {
          start: {
            line: 79,
            column: 4
          },
          end: {
            line: 81,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 79,
            column: 4
          },
          end: {
            line: 81,
            column: 5
          }
        }, {
          start: {
            line: 79,
            column: 4
          },
          end: {
            line: 81,
            column: 5
          }
        }],
        line: 79
      },
      '5': {
        loc: {
          start: {
            line: 83,
            column: 4
          },
          end: {
            line: 85,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 83,
            column: 4
          },
          end: {
            line: 85,
            column: 5
          }
        }, {
          start: {
            line: 83,
            column: 4
          },
          end: {
            line: 85,
            column: 5
          }
        }],
        line: 83
      },
      '6': {
        loc: {
          start: {
            line: 83,
            column: 8
          },
          end: {
            line: 83,
            column: 63
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 83,
            column: 8
          },
          end: {
            line: 83,
            column: 20
          }
        }, {
          start: {
            line: 83,
            column: 24
          },
          end: {
            line: 83,
            column: 36
          }
        }, {
          start: {
            line: 83,
            column: 40
          },
          end: {
            line: 83,
            column: 63
          }
        }],
        line: 83
      },
      '7': {
        loc: {
          start: {
            line: 94,
            column: 2
          },
          end: {
            line: 94,
            column: 40
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 94,
            column: 2
          },
          end: {
            line: 94,
            column: 40
          }
        }, {
          start: {
            line: 94,
            column: 2
          },
          end: {
            line: 94,
            column: 40
          }
        }],
        line: 94
      },
      '8': {
        loc: {
          start: {
            line: 95,
            column: 2
          },
          end: {
            line: 97,
            column: 3
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 95,
            column: 2
          },
          end: {
            line: 97,
            column: 3
          }
        }, {
          start: {
            line: 95,
            column: 2
          },
          end: {
            line: 97,
            column: 3
          }
        }],
        line: 95
      },
      '9': {
        loc: {
          start: {
            line: 112,
            column: 2
          },
          end: {
            line: 114,
            column: 3
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 112,
            column: 2
          },
          end: {
            line: 114,
            column: 3
          }
        }, {
          start: {
            line: 112,
            column: 2
          },
          end: {
            line: 114,
            column: 3
          }
        }],
        line: 112
      },
      '10': {
        loc: {
          start: {
            line: 134,
            column: 32
          },
          end: {
            line: 134,
            column: 45
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 134,
            column: 40
          },
          end: {
            line: 134,
            column: 41
          }
        }, {
          start: {
            line: 134,
            column: 44
          },
          end: {
            line: 134,
            column: 45
          }
        }],
        line: 134
      },
      '11': {
        loc: {
          start: {
            line: 142,
            column: 32
          },
          end: {
            line: 142,
            column: 45
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 142,
            column: 40
          },
          end: {
            line: 142,
            column: 41
          }
        }, {
          start: {
            line: 142,
            column: 44
          },
          end: {
            line: 142,
            column: 45
          }
        }],
        line: 142
      },
      '12': {
        loc: {
          start: {
            line: 161,
            column: 24
          },
          end: {
            line: 161,
            column: 78
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 161,
            column: 24
          },
          end: {
            line: 161,
            column: 46
          }
        }, {
          start: {
            line: 161,
            column: 50
          },
          end: {
            line: 161,
            column: 62
          }
        }, {
          start: {
            line: 161,
            column: 66
          },
          end: {
            line: 161,
            column: 78
          }
        }],
        line: 161
      },
      '13': {
        loc: {
          start: {
            line: 162,
            column: 4
          },
          end: {
            line: 162,
            column: 24
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 162,
            column: 4
          },
          end: {
            line: 162,
            column: 24
          }
        }, {
          start: {
            line: 162,
            column: 4
          },
          end: {
            line: 162,
            column: 24
          }
        }],
        line: 162
      },
      '14': {
        loc: {
          start: {
            line: 169,
            column: 24
          },
          end: {
            line: 169,
            column: 78
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 169,
            column: 24
          },
          end: {
            line: 169,
            column: 46
          }
        }, {
          start: {
            line: 169,
            column: 50
          },
          end: {
            line: 169,
            column: 62
          }
        }, {
          start: {
            line: 169,
            column: 66
          },
          end: {
            line: 169,
            column: 78
          }
        }],
        line: 169
      },
      '15': {
        loc: {
          start: {
            line: 170,
            column: 4
          },
          end: {
            line: 170,
            column: 25
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 170,
            column: 4
          },
          end: {
            line: 170,
            column: 25
          }
        }, {
          start: {
            line: 170,
            column: 4
          },
          end: {
            line: 170,
            column: 25
          }
        }],
        line: 170
      },
      '16': {
        loc: {
          start: {
            line: 192,
            column: 4
          },
          end: {
            line: 199,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 192,
            column: 4
          },
          end: {
            line: 199,
            column: 5
          }
        }, {
          start: {
            line: 192,
            column: 4
          },
          end: {
            line: 199,
            column: 5
          }
        }],
        line: 192
      },
      '17': {
        loc: {
          start: {
            line: 194,
            column: 6
          },
          end: {
            line: 196,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 194,
            column: 6
          },
          end: {
            line: 196,
            column: 7
          }
        }, {
          start: {
            line: 194,
            column: 6
          },
          end: {
            line: 196,
            column: 7
          }
        }],
        line: 194
      },
      '18': {
        loc: {
          start: {
            line: 201,
            column: 21
          },
          end: {
            line: 201,
            column: 49
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 201,
            column: 21
          },
          end: {
            line: 201,
            column: 33
          }
        }, {
          start: {
            line: 201,
            column: 37
          },
          end: {
            line: 201,
            column: 49
          }
        }],
        line: 201
      },
      '19': {
        loc: {
          start: {
            line: 202,
            column: 4
          },
          end: {
            line: 207,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 202,
            column: 4
          },
          end: {
            line: 207,
            column: 5
          }
        }, {
          start: {
            line: 202,
            column: 4
          },
          end: {
            line: 207,
            column: 5
          }
        }],
        line: 202
      },
      '20': {
        loc: {
          start: {
            line: 203,
            column: 6
          },
          end: {
            line: 205,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 203,
            column: 6
          },
          end: {
            line: 205,
            column: 7
          }
        }, {
          start: {
            line: 203,
            column: 6
          },
          end: {
            line: 205,
            column: 7
          }
        }],
        line: 203
      },
      '21': {
        loc: {
          start: {
            line: 210,
            column: 4
          },
          end: {
            line: 217,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 210,
            column: 4
          },
          end: {
            line: 217,
            column: 5
          }
        }, {
          start: {
            line: 210,
            column: 4
          },
          end: {
            line: 217,
            column: 5
          }
        }],
        line: 210
      },
      '22': {
        loc: {
          start: {
            line: 211,
            column: 6
          },
          end: {
            line: 213,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 211,
            column: 6
          },
          end: {
            line: 213,
            column: 7
          }
        }, {
          start: {
            line: 211,
            column: 6
          },
          end: {
            line: 213,
            column: 7
          }
        }],
        line: 211
      },
      '23': {
        loc: {
          start: {
            line: 219,
            column: 25
          },
          end: {
            line: 219,
            column: 54
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 219,
            column: 25
          },
          end: {
            line: 219,
            column: 38
          }
        }, {
          start: {
            line: 219,
            column: 42
          },
          end: {
            line: 219,
            column: 54
          }
        }],
        line: 219
      },
      '24': {
        loc: {
          start: {
            line: 220,
            column: 4
          },
          end: {
            line: 224,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 220,
            column: 4
          },
          end: {
            line: 224,
            column: 5
          }
        }, {
          start: {
            line: 220,
            column: 4
          },
          end: {
            line: 224,
            column: 5
          }
        }],
        line: 220
      },
      '25': {
        loc: {
          start: {
            line: 235,
            column: 4
          },
          end: {
            line: 253,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 235,
            column: 4
          },
          end: {
            line: 253,
            column: 5
          }
        }, {
          start: {
            line: 235,
            column: 4
          },
          end: {
            line: 253,
            column: 5
          }
        }],
        line: 235
      },
      '26': {
        loc: {
          start: {
            line: 237,
            column: 6
          },
          end: {
            line: 252,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 237,
            column: 6
          },
          end: {
            line: 252,
            column: 7
          }
        }, {
          start: {
            line: 237,
            column: 6
          },
          end: {
            line: 252,
            column: 7
          }
        }],
        line: 237
      },
      '27': {
        loc: {
          start: {
            line: 237,
            column: 10
          },
          end: {
            line: 237,
            column: 51
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 237,
            column: 10
          },
          end: {
            line: 237,
            column: 20
          }
        }, {
          start: {
            line: 237,
            column: 24
          },
          end: {
            line: 237,
            column: 51
          }
        }],
        line: 237
      },
      '28': {
        loc: {
          start: {
            line: 238,
            column: 8
          },
          end: {
            line: 243,
            column: 9
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 238,
            column: 8
          },
          end: {
            line: 243,
            column: 9
          }
        }, {
          start: {
            line: 238,
            column: 8
          },
          end: {
            line: 243,
            column: 9
          }
        }],
        line: 238
      },
      '29': {
        loc: {
          start: {
            line: 246,
            column: 8
          },
          end: {
            line: 251,
            column: 9
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 246,
            column: 8
          },
          end: {
            line: 251,
            column: 9
          }
        }, {
          start: {
            line: 246,
            column: 8
          },
          end: {
            line: 251,
            column: 9
          }
        }],
        line: 246
      },
      '30': {
        loc: {
          start: {
            line: 254,
            column: 4
          },
          end: {
            line: 266,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 254,
            column: 4
          },
          end: {
            line: 266,
            column: 5
          }
        }, {
          start: {
            line: 254,
            column: 4
          },
          end: {
            line: 266,
            column: 5
          }
        }],
        line: 254
      },
      '31': {
        loc: {
          start: {
            line: 256,
            column: 6
          },
          end: {
            line: 261,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 256,
            column: 6
          },
          end: {
            line: 261,
            column: 7
          }
        }, {
          start: {
            line: 256,
            column: 6
          },
          end: {
            line: 261,
            column: 7
          }
        }],
        line: 256
      },
      '32': {
        loc: {
          start: {
            line: 256,
            column: 10
          },
          end: {
            line: 256,
            column: 56
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 256,
            column: 10
          },
          end: {
            line: 256,
            column: 20
          }
        }, {
          start: {
            line: 256,
            column: 24
          },
          end: {
            line: 256,
            column: 56
          }
        }],
        line: 256
      },
      '33': {
        loc: {
          start: {
            line: 281,
            column: 4
          },
          end: {
            line: 284,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 281,
            column: 4
          },
          end: {
            line: 284,
            column: 5
          }
        }, {
          start: {
            line: 281,
            column: 4
          },
          end: {
            line: 284,
            column: 5
          }
        }],
        line: 281
      },
      '34': {
        loc: {
          start: {
            line: 290,
            column: 4
          },
          end: {
            line: 293,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 290,
            column: 4
          },
          end: {
            line: 293,
            column: 5
          }
        }, {
          start: {
            line: 290,
            column: 4
          },
          end: {
            line: 293,
            column: 5
          }
        }],
        line: 290
      },
      '35': {
        loc: {
          start: {
            line: 295,
            column: 4
          },
          end: {
            line: 306,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 295,
            column: 4
          },
          end: {
            line: 306,
            column: 5
          }
        }, {
          start: {
            line: 295,
            column: 4
          },
          end: {
            line: 306,
            column: 5
          }
        }],
        line: 295
      }
    },
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0,
      '6': 0,
      '7': 0,
      '8': 0,
      '9': 0,
      '10': 0,
      '11': 0,
      '12': 0,
      '13': 0,
      '14': 0,
      '15': 0,
      '16': 0,
      '17': 0,
      '18': 0,
      '19': 0,
      '20': 0,
      '21': 0,
      '22': 0,
      '23': 0,
      '24': 0,
      '25': 0,
      '26': 0,
      '27': 0,
      '28': 0,
      '29': 0,
      '30': 0,
      '31': 0,
      '32': 0,
      '33': 0,
      '34': 0,
      '35': 0,
      '36': 0,
      '37': 0,
      '38': 0,
      '39': 0,
      '40': 0,
      '41': 0,
      '42': 0,
      '43': 0,
      '44': 0,
      '45': 0,
      '46': 0,
      '47': 0,
      '48': 0,
      '49': 0,
      '50': 0,
      '51': 0,
      '52': 0,
      '53': 0,
      '54': 0,
      '55': 0,
      '56': 0,
      '57': 0,
      '58': 0,
      '59': 0,
      '60': 0,
      '61': 0,
      '62': 0,
      '63': 0,
      '64': 0,
      '65': 0,
      '66': 0,
      '67': 0,
      '68': 0,
      '69': 0,
      '70': 0,
      '71': 0,
      '72': 0,
      '73': 0,
      '74': 0,
      '75': 0,
      '76': 0,
      '77': 0,
      '78': 0,
      '79': 0,
      '80': 0,
      '81': 0,
      '82': 0,
      '83': 0,
      '84': 0,
      '85': 0,
      '86': 0,
      '87': 0,
      '88': 0,
      '89': 0,
      '90': 0,
      '91': 0,
      '92': 0,
      '93': 0,
      '94': 0,
      '95': 0,
      '96': 0,
      '97': 0,
      '98': 0,
      '99': 0,
      '100': 0,
      '101': 0,
      '102': 0,
      '103': 0,
      '104': 0,
      '105': 0,
      '106': 0,
      '107': 0,
      '108': 0,
      '109': 0,
      '110': 0,
      '111': 0,
      '112': 0,
      '113': 0,
      '114': 0,
      '115': 0,
      '116': 0,
      '117': 0,
      '118': 0,
      '119': 0,
      '120': 0,
      '121': 0,
      '122': 0,
      '123': 0,
      '124': 0,
      '125': 0,
      '126': 0,
      '127': 0,
      '128': 0,
      '129': 0,
      '130': 0,
      '131': 0,
      '132': 0,
      '133': 0,
      '134': 0,
      '135': 0,
      '136': 0,
      '137': 0,
      '138': 0,
      '139': 0,
      '140': 0,
      '141': 0,
      '142': 0,
      '143': 0,
      '144': 0,
      '145': 0,
      '146': 0,
      '147': 0,
      '148': 0,
      '149': 0,
      '150': 0,
      '151': 0,
      '152': 0,
      '153': 0,
      '154': 0,
      '155': 0,
      '156': 0,
      '157': 0,
      '158': 0,
      '159': 0,
      '160': 0,
      '161': 0,
      '162': 0,
      '163': 0,
      '164': 0,
      '165': 0,
      '166': 0,
      '167': 0,
      '168': 0,
      '169': 0,
      '170': 0,
      '171': 0,
      '172': 0,
      '173': 0,
      '174': 0,
      '175': 0,
      '176': 0,
      '177': 0,
      '178': 0,
      '179': 0,
      '180': 0,
      '181': 0,
      '182': 0
    },
    f: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0,
      '6': 0,
      '7': 0,
      '8': 0,
      '9': 0,
      '10': 0,
      '11': 0,
      '12': 0,
      '13': 0
    },
    b: {
      '0': [0, 0],
      '1': [0, 0],
      '2': [0, 0],
      '3': [0, 0],
      '4': [0, 0],
      '5': [0, 0],
      '6': [0, 0, 0],
      '7': [0, 0],
      '8': [0, 0],
      '9': [0, 0],
      '10': [0, 0],
      '11': [0, 0],
      '12': [0, 0, 0],
      '13': [0, 0],
      '14': [0, 0, 0],
      '15': [0, 0],
      '16': [0, 0],
      '17': [0, 0],
      '18': [0, 0],
      '19': [0, 0],
      '20': [0, 0],
      '21': [0, 0],
      '22': [0, 0],
      '23': [0, 0],
      '24': [0, 0],
      '25': [0, 0],
      '26': [0, 0],
      '27': [0, 0],
      '28': [0, 0],
      '29': [0, 0],
      '30': [0, 0],
      '31': [0, 0],
      '32': [0, 0],
      '33': [0, 0],
      '34': [0, 0],
      '35': [0, 0]
    },
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.feedPosition = feedPosition;
exports.jumpPosition = jumpPosition;
exports.makeInitialPosition = makeInitialPosition;
exports.copyPosition = copyPosition;
exports.default = lexer;
exports.lex = lex;
exports.findTextEnd = findTextEnd;
exports.lexText = lexText;
exports.lexComment = lexComment;
exports.lexTag = lexTag;
exports.isWhitespaceChar = isWhitespaceChar;
exports.lexTagName = lexTagName;
exports.lexTagAttributes = lexTagAttributes;
exports.lexSkipTag = lexSkipTag;

var _compat = require('./compat');

function feedPosition(position, str, len) {
  cov_1mknr9mehe.f[0]++;

  var start = (cov_1mknr9mehe.s[0]++, position.index);
  var end = (cov_1mknr9mehe.s[1]++, position.index = start + len);
  cov_1mknr9mehe.s[2]++;
  for (var i = start; i < end; i++) {
    var char = (cov_1mknr9mehe.s[3]++, str.charAt(i));
    cov_1mknr9mehe.s[4]++;
    if (char === '\n') {
      cov_1mknr9mehe.b[0][0]++;
      cov_1mknr9mehe.s[5]++;

      position.line++;
      cov_1mknr9mehe.s[6]++;
      position.column = 0;
    } else {
      cov_1mknr9mehe.b[0][1]++;
      cov_1mknr9mehe.s[7]++;

      position.column++;
    }
  }
}

function jumpPosition(position, str, end) {
  cov_1mknr9mehe.f[1]++;

  var len = (cov_1mknr9mehe.s[8]++, end - position.index);
  cov_1mknr9mehe.s[9]++;
  return feedPosition(position, str, len);
}

function makeInitialPosition() {
  cov_1mknr9mehe.f[2]++;
  cov_1mknr9mehe.s[10]++;

  return {
    index: 0,
    column: 0,
    line: 0
  };
}

function copyPosition(position) {
  cov_1mknr9mehe.f[3]++;
  cov_1mknr9mehe.s[11]++;

  return {
    index: position.index,
    line: position.line,
    column: position.column
  };
}

function lexer(str, options) {
  cov_1mknr9mehe.f[4]++;

  var state = (cov_1mknr9mehe.s[12]++, {
    str: str,
    options: options,
    position: makeInitialPosition(),
    tokens: []
  });
  cov_1mknr9mehe.s[13]++;
  lex(state);
  cov_1mknr9mehe.s[14]++;
  return state.tokens;
}

function lex(state) {
  cov_1mknr9mehe.f[5]++;

  var _ref = (cov_1mknr9mehe.s[15]++, state),
      str = _ref.str,
      childlessTags = _ref.options.childlessTags;

  var len = (cov_1mknr9mehe.s[16]++, str.length);
  cov_1mknr9mehe.s[17]++;
  while (state.position.index < len) {
    var start = (cov_1mknr9mehe.s[18]++, state.position.index);
    cov_1mknr9mehe.s[19]++;
    lexText(state);
    cov_1mknr9mehe.s[20]++;
    if (state.position.index === start) {
      cov_1mknr9mehe.b[1][0]++;

      var isComment = (cov_1mknr9mehe.s[21]++, (0, _compat.startsWith)(str, '!--', start + 1));
      cov_1mknr9mehe.s[22]++;
      if (isComment) {
        cov_1mknr9mehe.b[2][0]++;
        cov_1mknr9mehe.s[23]++;

        lexComment(state);
      } else {
        cov_1mknr9mehe.b[2][1]++;

        var tagName = (cov_1mknr9mehe.s[24]++, lexTag(state));
        var safeTag = (cov_1mknr9mehe.s[25]++, tagName.toLowerCase());
        cov_1mknr9mehe.s[26]++;
        if ((0, _compat.arrayIncludes)(childlessTags, safeTag)) {
          cov_1mknr9mehe.b[3][0]++;
          cov_1mknr9mehe.s[27]++;

          lexSkipTag(tagName, state);
        } else {
          cov_1mknr9mehe.b[3][1]++;
        }
      }
    } else {
      cov_1mknr9mehe.b[1][1]++;
    }
  }
}

var alphanumeric = (cov_1mknr9mehe.s[28]++, /[A-Za-z0-9]/);
function findTextEnd(str, index) {
  cov_1mknr9mehe.f[6]++;
  cov_1mknr9mehe.s[29]++;

  while (true) {
    var textEnd = (cov_1mknr9mehe.s[30]++, str.indexOf('<', index));
    cov_1mknr9mehe.s[31]++;
    if (textEnd === -1) {
      cov_1mknr9mehe.b[4][0]++;
      cov_1mknr9mehe.s[32]++;

      return textEnd;
    } else {
      cov_1mknr9mehe.b[4][1]++;
    }
    var char = (cov_1mknr9mehe.s[33]++, str.charAt(textEnd + 1));
    cov_1mknr9mehe.s[34]++;
    if ((cov_1mknr9mehe.b[6][0]++, char === '/') || (cov_1mknr9mehe.b[6][1]++, char === '!') || (cov_1mknr9mehe.b[6][2]++, alphanumeric.test(char))) {
      cov_1mknr9mehe.b[5][0]++;
      cov_1mknr9mehe.s[35]++;

      return textEnd;
    } else {
      cov_1mknr9mehe.b[5][1]++;
    }
    cov_1mknr9mehe.s[36]++;
    index = textEnd + 1;
  }
}

function lexText(state) {
  cov_1mknr9mehe.f[7]++;

  var type = (cov_1mknr9mehe.s[37]++, 'text');

  var _ref2 = (cov_1mknr9mehe.s[38]++, state),
      str = _ref2.str,
      position = _ref2.position;

  var textEnd = (cov_1mknr9mehe.s[39]++, findTextEnd(str, position.index));
  cov_1mknr9mehe.s[40]++;
  if (textEnd === position.index) {
      cov_1mknr9mehe.b[7][0]++;
      cov_1mknr9mehe.s[41]++;
      return;
    } else {
    cov_1mknr9mehe.b[7][1]++;
  }cov_1mknr9mehe.s[42]++;
  if (textEnd === -1) {
    cov_1mknr9mehe.b[8][0]++;
    cov_1mknr9mehe.s[43]++;

    textEnd = str.length;
  } else {
    cov_1mknr9mehe.b[8][1]++;
  }

  var start = (cov_1mknr9mehe.s[44]++, copyPosition(position));
  var content = (cov_1mknr9mehe.s[45]++, str.slice(position.index, textEnd));
  cov_1mknr9mehe.s[46]++;
  jumpPosition(position, str, textEnd);
  var end = (cov_1mknr9mehe.s[47]++, copyPosition(position));
  cov_1mknr9mehe.s[48]++;
  state.tokens.push({ type: type, content: content, position: { start: start, end: end } });
}

function lexComment(state) {
  cov_1mknr9mehe.f[8]++;

  var _ref3 = (cov_1mknr9mehe.s[49]++, state),
      str = _ref3.str,
      position = _ref3.position;

  var start = (cov_1mknr9mehe.s[50]++, copyPosition(position));
  cov_1mknr9mehe.s[51]++;
  feedPosition(position, str, 4); // "<!--".length
  var contentEnd = (cov_1mknr9mehe.s[52]++, str.indexOf('-->', position.index));
  var commentEnd = (cov_1mknr9mehe.s[53]++, contentEnd + 3); // "-->".length
  cov_1mknr9mehe.s[54]++;
  if (contentEnd === -1) {
    cov_1mknr9mehe.b[9][0]++;
    cov_1mknr9mehe.s[55]++;

    contentEnd = commentEnd = str.length;
  } else {
    cov_1mknr9mehe.b[9][1]++;
  }

  var content = (cov_1mknr9mehe.s[56]++, str.slice(position.index, contentEnd));
  cov_1mknr9mehe.s[57]++;
  jumpPosition(position, str, commentEnd);
  cov_1mknr9mehe.s[58]++;
  state.tokens.push({
    type: 'comment',
    content: content,
    position: {
      start: start,
      end: copyPosition(position)
    }
  });
}

function lexTag(state) {
  cov_1mknr9mehe.f[9]++;

  var _ref4 = (cov_1mknr9mehe.s[59]++, state),
      str = _ref4.str,
      position = _ref4.position;

  {
    var secondChar = (cov_1mknr9mehe.s[60]++, str.charAt(position.index + 1));
    var close = (cov_1mknr9mehe.s[61]++, secondChar === '/');
    var start = (cov_1mknr9mehe.s[62]++, copyPosition(position));
    cov_1mknr9mehe.s[63]++;
    feedPosition(position, str, close ? (cov_1mknr9mehe.b[10][0]++, 2) : (cov_1mknr9mehe.b[10][1]++, 1));
    cov_1mknr9mehe.s[64]++;
    state.tokens.push({ type: 'tag-start', close: close, position: { start: start } });
  }
  var tagName = (cov_1mknr9mehe.s[65]++, lexTagName(state));
  cov_1mknr9mehe.s[66]++;
  lexTagAttributes(state);
  {
    var firstChar = (cov_1mknr9mehe.s[67]++, str.charAt(position.index));
    var _close = (cov_1mknr9mehe.s[68]++, firstChar === '/');
    cov_1mknr9mehe.s[69]++;
    feedPosition(position, str, _close ? (cov_1mknr9mehe.b[11][0]++, 2) : (cov_1mknr9mehe.b[11][1]++, 1));
    var end = (cov_1mknr9mehe.s[70]++, copyPosition(position));
    cov_1mknr9mehe.s[71]++;
    state.tokens.push({ type: 'tag-end', close: _close, position: { end: end } });
  }
  cov_1mknr9mehe.s[72]++;
  return tagName;
}

// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#special-white-space
var whitespace = (cov_1mknr9mehe.s[73]++, /\s/);
function isWhitespaceChar(char) {
  cov_1mknr9mehe.f[10]++;
  cov_1mknr9mehe.s[74]++;

  return whitespace.test(char);
}

function lexTagName(state) {
  cov_1mknr9mehe.f[11]++;

  var _ref5 = (cov_1mknr9mehe.s[75]++, state),
      str = _ref5.str,
      position = _ref5.position;

  var len = (cov_1mknr9mehe.s[76]++, str.length);
  var start = (cov_1mknr9mehe.s[77]++, position.index);
  cov_1mknr9mehe.s[78]++;
  while (start < len) {
    var char = (cov_1mknr9mehe.s[79]++, str.charAt(start));
    var isTagChar = (cov_1mknr9mehe.s[80]++, !((cov_1mknr9mehe.b[12][0]++, isWhitespaceChar(char)) || (cov_1mknr9mehe.b[12][1]++, char === '/') || (cov_1mknr9mehe.b[12][2]++, char === '>')));
    cov_1mknr9mehe.s[81]++;
    if (isTagChar) {
        cov_1mknr9mehe.b[13][0]++;
        cov_1mknr9mehe.s[82]++;
        break;
      } else {
      cov_1mknr9mehe.b[13][1]++;
    }cov_1mknr9mehe.s[83]++;
    start++;
  }

  var end = (cov_1mknr9mehe.s[84]++, start + 1);
  cov_1mknr9mehe.s[85]++;
  while (end < len) {
    var _char = (cov_1mknr9mehe.s[86]++, str.charAt(end));
    var _isTagChar = (cov_1mknr9mehe.s[87]++, !((cov_1mknr9mehe.b[14][0]++, isWhitespaceChar(_char)) || (cov_1mknr9mehe.b[14][1]++, _char === '/') || (cov_1mknr9mehe.b[14][2]++, _char === '>')));
    cov_1mknr9mehe.s[88]++;
    if (!_isTagChar) {
        cov_1mknr9mehe.b[15][0]++;
        cov_1mknr9mehe.s[89]++;
        break;
      } else {
      cov_1mknr9mehe.b[15][1]++;
    }cov_1mknr9mehe.s[90]++;
    end++;
  }

  cov_1mknr9mehe.s[91]++;
  jumpPosition(position, str, end);
  var tagName = (cov_1mknr9mehe.s[92]++, str.slice(start, end));
  cov_1mknr9mehe.s[93]++;
  state.tokens.push({
    type: 'tag',
    content: tagName
  });
  cov_1mknr9mehe.s[94]++;
  return tagName;
}

function lexTagAttributes(state) {
  cov_1mknr9mehe.f[12]++;

  var _ref6 = (cov_1mknr9mehe.s[95]++, state),
      str = _ref6.str,
      position = _ref6.position,
      tokens = _ref6.tokens;

  var cursor = (cov_1mknr9mehe.s[96]++, position.index);
  var quote = (cov_1mknr9mehe.s[97]++, null); // null, single-, or double-quote
  var wordBegin = (cov_1mknr9mehe.s[98]++, cursor); // index of word start
  var words = (cov_1mknr9mehe.s[99]++, []); // "key", "key=value", "key='value'", etc
  var len = (cov_1mknr9mehe.s[100]++, str.length);
  cov_1mknr9mehe.s[101]++;
  while (cursor < len) {
    var char = (cov_1mknr9mehe.s[102]++, str.charAt(cursor));
    cov_1mknr9mehe.s[103]++;
    if (quote) {
      cov_1mknr9mehe.b[16][0]++;

      var isQuoteEnd = (cov_1mknr9mehe.s[104]++, char === quote);
      cov_1mknr9mehe.s[105]++;
      if (isQuoteEnd) {
        cov_1mknr9mehe.b[17][0]++;
        cov_1mknr9mehe.s[106]++;

        quote = null;
      } else {
        cov_1mknr9mehe.b[17][1]++;
      }
      cov_1mknr9mehe.s[107]++;
      cursor++;
      cov_1mknr9mehe.s[108]++;
      continue;
    } else {
      cov_1mknr9mehe.b[16][1]++;
    }

    var isTagEnd = (cov_1mknr9mehe.s[109]++, (cov_1mknr9mehe.b[18][0]++, char === '/') || (cov_1mknr9mehe.b[18][1]++, char === '>'));
    cov_1mknr9mehe.s[110]++;
    if (isTagEnd) {
      cov_1mknr9mehe.b[19][0]++;
      cov_1mknr9mehe.s[111]++;

      if (cursor !== wordBegin) {
        cov_1mknr9mehe.b[20][0]++;
        cov_1mknr9mehe.s[112]++;

        words.push(str.slice(wordBegin, cursor));
      } else {
        cov_1mknr9mehe.b[20][1]++;
      }
      cov_1mknr9mehe.s[113]++;
      break;
    } else {
      cov_1mknr9mehe.b[19][1]++;
    }

    var isWordEnd = (cov_1mknr9mehe.s[114]++, isWhitespaceChar(char));
    cov_1mknr9mehe.s[115]++;
    if (isWordEnd) {
      cov_1mknr9mehe.b[21][0]++;
      cov_1mknr9mehe.s[116]++;

      if (cursor !== wordBegin) {
        cov_1mknr9mehe.b[22][0]++;
        cov_1mknr9mehe.s[117]++;

        words.push(str.slice(wordBegin, cursor));
      } else {
        cov_1mknr9mehe.b[22][1]++;
      }
      cov_1mknr9mehe.s[118]++;
      wordBegin = cursor + 1;
      cov_1mknr9mehe.s[119]++;
      cursor++;
      cov_1mknr9mehe.s[120]++;
      continue;
    } else {
      cov_1mknr9mehe.b[21][1]++;
    }

    var isQuoteStart = (cov_1mknr9mehe.s[121]++, (cov_1mknr9mehe.b[23][0]++, char === '\'') || (cov_1mknr9mehe.b[23][1]++, char === '"'));
    cov_1mknr9mehe.s[122]++;
    if (isQuoteStart) {
      cov_1mknr9mehe.b[24][0]++;
      cov_1mknr9mehe.s[123]++;

      quote = char;
      cov_1mknr9mehe.s[124]++;
      cursor++;
      cov_1mknr9mehe.s[125]++;
      continue;
    } else {
      cov_1mknr9mehe.b[24][1]++;
    }

    cov_1mknr9mehe.s[126]++;
    cursor++;
  }
  cov_1mknr9mehe.s[127]++;
  jumpPosition(position, str, cursor);

  var wLen = (cov_1mknr9mehe.s[128]++, words.length);
  var type = (cov_1mknr9mehe.s[129]++, 'attribute');
  cov_1mknr9mehe.s[130]++;
  for (var i = 0; i < wLen; i++) {
    var word = (cov_1mknr9mehe.s[131]++, words[i]);
    var isNotPair = (cov_1mknr9mehe.s[132]++, word.indexOf('=') === -1);
    cov_1mknr9mehe.s[133]++;
    if (isNotPair) {
      cov_1mknr9mehe.b[25][0]++;

      var secondWord = (cov_1mknr9mehe.s[134]++, words[i + 1]);
      cov_1mknr9mehe.s[135]++;
      if ((cov_1mknr9mehe.b[27][0]++, secondWord) && (cov_1mknr9mehe.b[27][1]++, (0, _compat.startsWith)(secondWord, '='))) {
        cov_1mknr9mehe.b[26][0]++;
        cov_1mknr9mehe.s[136]++;

        if (secondWord.length > 1) {
          cov_1mknr9mehe.b[28][0]++;

          var newWord = (cov_1mknr9mehe.s[137]++, word + secondWord);
          cov_1mknr9mehe.s[138]++;
          tokens.push({ type: type, content: newWord });
          cov_1mknr9mehe.s[139]++;
          i += 1;
          cov_1mknr9mehe.s[140]++;
          continue;
        } else {
          cov_1mknr9mehe.b[28][1]++;
        }
        var thirdWord = (cov_1mknr9mehe.s[141]++, words[i + 2]);
        cov_1mknr9mehe.s[142]++;
        i += 1;
        cov_1mknr9mehe.s[143]++;
        if (thirdWord) {
          cov_1mknr9mehe.b[29][0]++;

          var _newWord = (cov_1mknr9mehe.s[144]++, word + '=' + thirdWord);
          cov_1mknr9mehe.s[145]++;
          tokens.push({ type: type, content: _newWord });
          cov_1mknr9mehe.s[146]++;
          i += 1;
          cov_1mknr9mehe.s[147]++;
          continue;
        } else {
          cov_1mknr9mehe.b[29][1]++;
        }
      } else {
        cov_1mknr9mehe.b[26][1]++;
      }
    } else {
      cov_1mknr9mehe.b[25][1]++;
    }
    cov_1mknr9mehe.s[148]++;
    if ((0, _compat.endsWith)(word, '=')) {
      cov_1mknr9mehe.b[30][0]++;

      var _secondWord = (cov_1mknr9mehe.s[149]++, words[i + 1]);
      cov_1mknr9mehe.s[150]++;
      if ((cov_1mknr9mehe.b[32][0]++, _secondWord) && (cov_1mknr9mehe.b[32][1]++, !(0, _compat.stringIncludes)(_secondWord, '='))) {
        cov_1mknr9mehe.b[31][0]++;

        var _newWord3 = (cov_1mknr9mehe.s[151]++, word + _secondWord);
        cov_1mknr9mehe.s[152]++;
        tokens.push({ type: type, content: _newWord3 });
        cov_1mknr9mehe.s[153]++;
        i += 1;
        cov_1mknr9mehe.s[154]++;
        continue;
      } else {
        cov_1mknr9mehe.b[31][1]++;
      }

      var _newWord2 = (cov_1mknr9mehe.s[155]++, word.slice(0, -1));
      cov_1mknr9mehe.s[156]++;
      tokens.push({ type: type, content: _newWord2 });
      cov_1mknr9mehe.s[157]++;
      continue;
    } else {
      cov_1mknr9mehe.b[30][1]++;
    }

    cov_1mknr9mehe.s[158]++;
    tokens.push({ type: type, content: word });
  }
}

var push = (cov_1mknr9mehe.s[159]++, [].push);

function lexSkipTag(tagName, state) {
  cov_1mknr9mehe.f[13]++;

  var _ref7 = (cov_1mknr9mehe.s[160]++, state),
      str = _ref7.str,
      position = _ref7.position,
      tokens = _ref7.tokens;

  var safeTagName = (cov_1mknr9mehe.s[161]++, tagName.toLowerCase());
  var len = (cov_1mknr9mehe.s[162]++, str.length);
  var index = (cov_1mknr9mehe.s[163]++, position.index);
  cov_1mknr9mehe.s[164]++;
  while (index < len) {
    var nextTag = (cov_1mknr9mehe.s[165]++, str.indexOf('</', index));
    cov_1mknr9mehe.s[166]++;
    if (nextTag === -1) {
      cov_1mknr9mehe.b[33][0]++;
      cov_1mknr9mehe.s[167]++;

      lexText(state);
      cov_1mknr9mehe.s[168]++;
      break;
    } else {
      cov_1mknr9mehe.b[33][1]++;
    }

    var tagStartPosition = (cov_1mknr9mehe.s[169]++, copyPosition(position));
    cov_1mknr9mehe.s[170]++;
    jumpPosition(tagStartPosition, str, nextTag);
    var tagState = (cov_1mknr9mehe.s[171]++, { str: str, position: tagStartPosition, tokens: [] });
    var name = (cov_1mknr9mehe.s[172]++, lexTag(tagState));
    cov_1mknr9mehe.s[173]++;
    if (safeTagName !== name.toLowerCase()) {
      cov_1mknr9mehe.b[34][0]++;
      cov_1mknr9mehe.s[174]++;

      index = tagState.position.index;
      cov_1mknr9mehe.s[175]++;
      continue;
    } else {
      cov_1mknr9mehe.b[34][1]++;
    }

    cov_1mknr9mehe.s[176]++;
    if (nextTag !== position.index) {
      cov_1mknr9mehe.b[35][0]++;

      var textStart = (cov_1mknr9mehe.s[177]++, copyPosition(position));
      cov_1mknr9mehe.s[178]++;
      jumpPosition(position, str, nextTag);
      cov_1mknr9mehe.s[179]++;
      tokens.push({
        type: 'text',
        content: str.slice(textStart.index, nextTag),
        position: {
          start: textStart,
          end: copyPosition(position)
        }
      });
    } else {
      cov_1mknr9mehe.b[35][1]++;
    }

    cov_1mknr9mehe.s[180]++;
    push.apply(tokens, tagState.tokens);
    cov_1mknr9mehe.s[181]++;
    jumpPosition(position, str, tagState.position.index);
    cov_1mknr9mehe.s[182]++;
    break;
  }
}

},{"./compat":1}],5:[function(require,module,exports){
'use strict';

var cov_q4ngc1js5 = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/parser.js',
      hash = '10fb0478bb046c7059c47c8225586b7e30f48474',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/parser.js',
    statementMap: {
      '0': {
        start: {
          line: 4,
          column: 15
        },
        end: {
          line: 4,
          column: 44
        }
      },
      '1': {
        start: {
          line: 5,
          column: 16
        },
        end: {
          line: 5,
          column: 59
        }
      },
      '2': {
        start: {
          line: 6,
          column: 2
        },
        end: {
          line: 6,
          column: 14
        }
      },
      '3': {
        start: {
          line: 7,
          column: 2
        },
        end: {
          line: 7,
          column: 22
        }
      },
      '4': {
        start: {
          line: 11,
          column: 21
        },
        end: {
          line: 11,
          column: 39
        }
      },
      '5': {
        start: {
          line: 12,
          column: 2
        },
        end: {
          line: 24,
          column: 3
        }
      },
      '6': {
        start: {
          line: 13,
          column: 23
        },
        end: {
          line: 13,
          column: 39
        }
      },
      '7': {
        start: {
          line: 14,
          column: 4
        },
        end: {
          line: 23,
          column: 5
        }
      },
      '8': {
        start: {
          line: 15,
          column: 28
        },
        end: {
          line: 15,
          column: 55
        }
      },
      '9': {
        start: {
          line: 16,
          column: 6
        },
        end: {
          line: 18,
          column: 7
        }
      },
      '10': {
        start: {
          line: 17,
          column: 8
        },
        end: {
          line: 17,
          column: 13
        }
      },
      '11': {
        start: {
          line: 19,
          column: 6
        },
        end: {
          line: 21,
          column: 7
        }
      },
      '12': {
        start: {
          line: 20,
          column: 8
        },
        end: {
          line: 20,
          column: 19
        }
      },
      '13': {
        start: {
          line: 22,
          column: 6
        },
        end: {
          line: 22,
          column: 20
        }
      },
      '14': {
        start: {
          line: 25,
          column: 2
        },
        end: {
          line: 25,
          column: 14
        }
      },
      '15': {
        start: {
          line: 29,
          column: 2
        },
        end: {
          line: 29,
          column: 45
        }
      },
      '16': {
        start: {
          line: 30,
          column: 2
        },
        end: {
          line: 32,
          column: 3
        }
      },
      '17': {
        start: {
          line: 31,
          column: 4
        },
        end: {
          line: 31,
          column: 47
        }
      },
      '18': {
        start: {
          line: 33,
          column: 2
        },
        end: {
          line: 33,
          column: 25
        }
      },
      '19': {
        start: {
          line: 37,
          column: 28
        },
        end: {
          line: 37,
          column: 33
        }
      },
      '20': {
        start: {
          line: 38,
          column: 16
        },
        end: {
          line: 38,
          column: 21
        }
      },
      '21': {
        start: {
          line: 39,
          column: 14
        },
        end: {
          line: 39,
          column: 46
        }
      },
      '22': {
        start: {
          line: 40,
          column: 14
        },
        end: {
          line: 40,
          column: 27
        }
      },
      '23': {
        start: {
          line: 41,
          column: 17
        },
        end: {
          line: 41,
          column: 22
        }
      },
      '24': {
        start: {
          line: 42,
          column: 2
        },
        end: {
          line: 132,
          column: 3
        }
      },
      '25': {
        start: {
          line: 43,
          column: 18
        },
        end: {
          line: 43,
          column: 32
        }
      },
      '26': {
        start: {
          line: 44,
          column: 4
        },
        end: {
          line: 48,
          column: 5
        }
      },
      '27': {
        start: {
          line: 45,
          column: 6
        },
        end: {
          line: 45,
          column: 23
        }
      },
      '28': {
        start: {
          line: 46,
          column: 6
        },
        end: {
          line: 46,
          column: 14
        }
      },
      '29': {
        start: {
          line: 47,
          column: 6
        },
        end: {
          line: 47,
          column: 14
        }
      },
      '30': {
        start: {
          line: 50,
          column: 21
        },
        end: {
          line: 50,
          column: 37
        }
      },
      '31': {
        start: {
          line: 51,
          column: 4
        },
        end: {
          line: 51,
          column: 12
        }
      },
      '32': {
        start: {
          line: 52,
          column: 20
        },
        end: {
          line: 52,
          column: 50
        }
      },
      '33': {
        start: {
          line: 53,
          column: 4
        },
        end: {
          line: 73,
          column: 5
        }
      },
      '34': {
        start: {
          line: 54,
          column: 18
        },
        end: {
          line: 54,
          column: 30
        }
      },
      '35': {
        start: {
          line: 55,
          column: 25
        },
        end: {
          line: 55,
          column: 30
        }
      },
      '36': {
        start: {
          line: 56,
          column: 6
        },
        end: {
          line: 61,
          column: 7
        }
      },
      '37': {
        start: {
          line: 57,
          column: 8
        },
        end: {
          line: 60,
          column: 9
        }
      },
      '38': {
        start: {
          line: 58,
          column: 10
        },
        end: {
          line: 58,
          column: 29
        }
      },
      '39': {
        start: {
          line: 59,
          column: 10
        },
        end: {
          line: 59,
          column: 15
        }
      },
      '40': {
        start: {
          line: 62,
          column: 6
        },
        end: {
          line: 66,
          column: 7
        }
      },
      '41': {
        start: {
          line: 63,
          column: 25
        },
        end: {
          line: 63,
          column: 39
        }
      },
      '42': {
        start: {
          line: 64,
          column: 8
        },
        end: {
          line: 64,
          column: 46
        }
      },
      '43': {
        start: {
          line: 64,
          column: 41
        },
        end: {
          line: 64,
          column: 46
        }
      },
      '44': {
        start: {
          line: 65,
          column: 8
        },
        end: {
          line: 65,
          column: 16
        }
      },
      '45': {
        start: {
          line: 67,
          column: 6
        },
        end: {
          line: 72,
          column: 7
        }
      },
      '46': {
        start: {
          line: 68,
          column: 8
        },
        end: {
          line: 68,
          column: 88
        }
      },
      '47': {
        start: {
          line: 69,
          column: 8
        },
        end: {
          line: 69,
          column: 13
        }
      },
      '48': {
        start: {
          line: 71,
          column: 8
        },
        end: {
          line: 71,
          column: 16
        }
      },
      '49': {
        start: {
          line: 75,
          column: 25
        },
        end: {
          line: 75,
          column: 68
        }
      },
      '50': {
        start: {
          line: 76,
          column: 34
        },
        end: {
          line: 76,
          column: 46
        }
      },
      '51': {
        start: {
          line: 77,
          column: 4
        },
        end: {
          line: 80,
          column: 5
        }
      },
      '52': {
        start: {
          line: 78,
          column: 56
        },
        end: {
          line: 78,
          column: 63
        }
      },
      '53': {
        start: {
          line: 79,
          column: 6
        },
        end: {
          line: 79,
          column: 77
        }
      },
      '54': {
        start: {
          line: 82,
          column: 4
        },
        end: {
          line: 95,
          column: 5
        }
      },
      '55': {
        start: {
          line: 85,
          column: 25
        },
        end: {
          line: 85,
          column: 41
        }
      },
      '56': {
        start: {
          line: 86,
          column: 6
        },
        end: {
          line: 94,
          column: 7
        }
      },
      '57': {
        start: {
          line: 87,
          column: 8
        },
        end: {
          line: 92,
          column: 9
        }
      },
      '58': {
        start: {
          line: 88,
          column: 10
        },
        end: {
          line: 88,
          column: 86
        }
      },
      '59': {
        start: {
          line: 89,
          column: 32
        },
        end: {
          line: 89,
          column: 48
        }
      },
      '60': {
        start: {
          line: 90,
          column: 10
        },
        end: {
          line: 90,
          column: 47
        }
      },
      '61': {
        start: {
          line: 91,
          column: 10
        },
        end: {
          line: 91,
          column: 15
        }
      },
      '62': {
        start: {
          line: 93,
          column: 8
        },
        end: {
          line: 93,
          column: 39
        }
      },
      '63': {
        start: {
          line: 97,
          column: 21
        },
        end: {
          line: 97,
          column: 23
        }
      },
      '64': {
        start: {
          line: 99,
          column: 4
        },
        end: {
          line: 104,
          column: 5
        }
      },
      '65': {
        start: {
          line: 100,
          column: 6
        },
        end: {
          line: 100,
          column: 32
        }
      },
      '66': {
        start: {
          line: 101,
          column: 6
        },
        end: {
          line: 101,
          column: 45
        }
      },
      '67': {
        start: {
          line: 101,
          column: 40
        },
        end: {
          line: 101,
          column: 45
        }
      },
      '68': {
        start: {
          line: 102,
          column: 6
        },
        end: {
          line: 102,
          column: 40
        }
      },
      '69': {
        start: {
          line: 103,
          column: 6
        },
        end: {
          line: 103,
          column: 14
        }
      },
      '70': {
        start: {
          line: 106,
          column: 4
        },
        end: {
          line: 106,
          column: 12
        }
      },
      '71': {
        start: {
          line: 107,
          column: 21
        },
        end: {
          line: 107,
          column: 23
        }
      },
      '72': {
        start: {
          line: 108,
          column: 21
        },
        end: {
          line: 111,
          column: 5
        }
      },
      '73': {
        start: {
          line: 112,
          column: 24
        },
        end: {
          line: 118,
          column: 5
        }
      },
      '74': {
        start: {
          line: 119,
          column: 4
        },
        end: {
          line: 119,
          column: 27
        }
      },
      '75': {
        start: {
          line: 121,
          column: 24
        },
        end: {
          line: 121,
          column: 86
        }
      },
      '76': {
        start: {
          line: 122,
          column: 4
        },
        end: {
          line: 131,
          column: 5
        }
      },
      '77': {
        start: {
          line: 123,
          column: 19
        },
        end: {
          line: 123,
          column: 60
        }
      },
      '78': {
        start: {
          line: 124,
          column: 25
        },
        end: {
          line: 124,
          column: 57
        }
      },
      '79': {
        start: {
          line: 125,
          column: 6
        },
        end: {
          line: 125,
          column: 23
        }
      },
      '80': {
        start: {
          line: 126,
          column: 6
        },
        end: {
          line: 126,
          column: 32
        }
      },
      '81': {
        start: {
          line: 127,
          column: 31
        },
        end: {
          line: 127,
          column: 52
        }
      },
      '82': {
        start: {
          line: 128,
          column: 6
        },
        end: {
          line: 130,
          column: 7
        }
      },
      '83': {
        start: {
          line: 129,
          column: 8
        },
        end: {
          line: 129,
          column: 66
        }
      },
      '84': {
        start: {
          line: 133,
          column: 2
        },
        end: {
          line: 133,
          column: 23
        }
      }
    },
    fnMap: {
      '0': {
        name: 'parser',
        decl: {
          start: {
            line: 3,
            column: 24
          },
          end: {
            line: 3,
            column: 30
          }
        },
        loc: {
          start: {
            line: 3,
            column: 49
          },
          end: {
            line: 8,
            column: 1
          }
        },
        line: 3
      },
      '1': {
        name: 'hasTerminalParent',
        decl: {
          start: {
            line: 10,
            column: 16
          },
          end: {
            line: 10,
            column: 33
          }
        },
        loc: {
          start: {
            line: 10,
            column: 62
          },
          end: {
            line: 26,
            column: 1
          }
        },
        line: 10
      },
      '2': {
        name: 'rewindStack',
        decl: {
          start: {
            line: 28,
            column: 16
          },
          end: {
            line: 28,
            column: 27
          }
        },
        loc: {
          start: {
            line: 28,
            column: 81
          },
          end: {
            line: 34,
            column: 1
          }
        },
        line: 28
      },
      '3': {
        name: 'parse',
        decl: {
          start: {
            line: 36,
            column: 16
          },
          end: {
            line: 36,
            column: 21
          }
        },
        loc: {
          start: {
            line: 36,
            column: 30
          },
          end: {
            line: 134,
            column: 1
          }
        },
        line: 36
      }
    },
    branchMap: {
      '0': {
        loc: {
          start: {
            line: 12,
            column: 2
          },
          end: {
            line: 24,
            column: 3
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 12,
            column: 2
          },
          end: {
            line: 24,
            column: 3
          }
        }, {
          start: {
            line: 12,
            column: 2
          },
          end: {
            line: 24,
            column: 3
          }
        }],
        line: 12
      },
      '1': {
        loc: {
          start: {
            line: 16,
            column: 6
          },
          end: {
            line: 18,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 16,
            column: 6
          },
          end: {
            line: 18,
            column: 7
          }
        }, {
          start: {
            line: 16,
            column: 6
          },
          end: {
            line: 18,
            column: 7
          }
        }],
        line: 16
      },
      '2': {
        loc: {
          start: {
            line: 19,
            column: 6
          },
          end: {
            line: 21,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 19,
            column: 6
          },
          end: {
            line: 21,
            column: 7
          }
        }, {
          start: {
            line: 19,
            column: 6
          },
          end: {
            line: 21,
            column: 7
          }
        }],
        line: 19
      },
      '3': {
        loc: {
          start: {
            line: 44,
            column: 4
          },
          end: {
            line: 48,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 44,
            column: 4
          },
          end: {
            line: 48,
            column: 5
          }
        }, {
          start: {
            line: 44,
            column: 4
          },
          end: {
            line: 48,
            column: 5
          }
        }],
        line: 44
      },
      '4': {
        loc: {
          start: {
            line: 53,
            column: 4
          },
          end: {
            line: 73,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 53,
            column: 4
          },
          end: {
            line: 73,
            column: 5
          }
        }, {
          start: {
            line: 53,
            column: 4
          },
          end: {
            line: 73,
            column: 5
          }
        }],
        line: 53
      },
      '5': {
        loc: {
          start: {
            line: 57,
            column: 8
          },
          end: {
            line: 60,
            column: 9
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 57,
            column: 8
          },
          end: {
            line: 60,
            column: 9
          }
        }, {
          start: {
            line: 57,
            column: 8
          },
          end: {
            line: 60,
            column: 9
          }
        }],
        line: 57
      },
      '6': {
        loc: {
          start: {
            line: 64,
            column: 8
          },
          end: {
            line: 64,
            column: 46
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 64,
            column: 8
          },
          end: {
            line: 64,
            column: 46
          }
        }, {
          start: {
            line: 64,
            column: 8
          },
          end: {
            line: 64,
            column: 46
          }
        }],
        line: 64
      },
      '7': {
        loc: {
          start: {
            line: 67,
            column: 6
          },
          end: {
            line: 72,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 67,
            column: 6
          },
          end: {
            line: 72,
            column: 7
          }
        }, {
          start: {
            line: 67,
            column: 6
          },
          end: {
            line: 72,
            column: 7
          }
        }],
        line: 67
      },
      '8': {
        loc: {
          start: {
            line: 77,
            column: 4
          },
          end: {
            line: 80,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 77,
            column: 4
          },
          end: {
            line: 80,
            column: 5
          }
        }, {
          start: {
            line: 77,
            column: 4
          },
          end: {
            line: 80,
            column: 5
          }
        }],
        line: 77
      },
      '9': {
        loc: {
          start: {
            line: 82,
            column: 4
          },
          end: {
            line: 95,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 82,
            column: 4
          },
          end: {
            line: 95,
            column: 5
          }
        }, {
          start: {
            line: 82,
            column: 4
          },
          end: {
            line: 95,
            column: 5
          }
        }],
        line: 82
      },
      '10': {
        loc: {
          start: {
            line: 87,
            column: 8
          },
          end: {
            line: 92,
            column: 9
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 87,
            column: 8
          },
          end: {
            line: 92,
            column: 9
          }
        }, {
          start: {
            line: 87,
            column: 8
          },
          end: {
            line: 92,
            column: 9
          }
        }],
        line: 87
      },
      '11': {
        loc: {
          start: {
            line: 101,
            column: 6
          },
          end: {
            line: 101,
            column: 45
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 101,
            column: 6
          },
          end: {
            line: 101,
            column: 45
          }
        }, {
          start: {
            line: 101,
            column: 6
          },
          end: {
            line: 101,
            column: 45
          }
        }],
        line: 101
      },
      '12': {
        loc: {
          start: {
            line: 121,
            column: 26
          },
          end: {
            line: 121,
            column: 85
          }
        },
        type: 'binary-expr',
        locations: [{
          start: {
            line: 121,
            column: 26
          },
          end: {
            line: 121,
            column: 41
          }
        }, {
          start: {
            line: 121,
            column: 45
          },
          end: {
            line: 121,
            column: 85
          }
        }],
        line: 121
      },
      '13': {
        loc: {
          start: {
            line: 122,
            column: 4
          },
          end: {
            line: 131,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 122,
            column: 4
          },
          end: {
            line: 131,
            column: 5
          }
        }, {
          start: {
            line: 122,
            column: 4
          },
          end: {
            line: 131,
            column: 5
          }
        }],
        line: 122
      },
      '14': {
        loc: {
          start: {
            line: 128,
            column: 6
          },
          end: {
            line: 130,
            column: 7
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 128,
            column: 6
          },
          end: {
            line: 130,
            column: 7
          }
        }, {
          start: {
            line: 128,
            column: 6
          },
          end: {
            line: 130,
            column: 7
          }
        }],
        line: 128
      }
    },
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0,
      '6': 0,
      '7': 0,
      '8': 0,
      '9': 0,
      '10': 0,
      '11': 0,
      '12': 0,
      '13': 0,
      '14': 0,
      '15': 0,
      '16': 0,
      '17': 0,
      '18': 0,
      '19': 0,
      '20': 0,
      '21': 0,
      '22': 0,
      '23': 0,
      '24': 0,
      '25': 0,
      '26': 0,
      '27': 0,
      '28': 0,
      '29': 0,
      '30': 0,
      '31': 0,
      '32': 0,
      '33': 0,
      '34': 0,
      '35': 0,
      '36': 0,
      '37': 0,
      '38': 0,
      '39': 0,
      '40': 0,
      '41': 0,
      '42': 0,
      '43': 0,
      '44': 0,
      '45': 0,
      '46': 0,
      '47': 0,
      '48': 0,
      '49': 0,
      '50': 0,
      '51': 0,
      '52': 0,
      '53': 0,
      '54': 0,
      '55': 0,
      '56': 0,
      '57': 0,
      '58': 0,
      '59': 0,
      '60': 0,
      '61': 0,
      '62': 0,
      '63': 0,
      '64': 0,
      '65': 0,
      '66': 0,
      '67': 0,
      '68': 0,
      '69': 0,
      '70': 0,
      '71': 0,
      '72': 0,
      '73': 0,
      '74': 0,
      '75': 0,
      '76': 0,
      '77': 0,
      '78': 0,
      '79': 0,
      '80': 0,
      '81': 0,
      '82': 0,
      '83': 0,
      '84': 0
    },
    f: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0
    },
    b: {
      '0': [0, 0],
      '1': [0, 0],
      '2': [0, 0],
      '3': [0, 0],
      '4': [0, 0],
      '5': [0, 0],
      '6': [0, 0],
      '7': [0, 0],
      '8': [0, 0],
      '9': [0, 0],
      '10': [0, 0],
      '11': [0, 0],
      '12': [0, 0],
      '13': [0, 0],
      '14': [0, 0]
    },
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = parser;
exports.hasTerminalParent = hasTerminalParent;
exports.rewindStack = rewindStack;
exports.parse = parse;

var _compat = require('./compat');

function parser(tokens, options) {
  cov_q4ngc1js5.f[0]++;

  var root = (cov_q4ngc1js5.s[0]++, { tagName: null, children: [] });
  var state = (cov_q4ngc1js5.s[1]++, { tokens: tokens, options: options, cursor: 0, stack: [root] });
  cov_q4ngc1js5.s[2]++;
  parse(state);
  cov_q4ngc1js5.s[3]++;
  return root.children;
}

function hasTerminalParent(tagName, stack, terminals) {
  cov_q4ngc1js5.f[1]++;

  var tagParents = (cov_q4ngc1js5.s[4]++, terminals[tagName]);
  cov_q4ngc1js5.s[5]++;
  if (tagParents) {
    cov_q4ngc1js5.b[0][0]++;

    var currentIndex = (cov_q4ngc1js5.s[6]++, stack.length - 1);
    cov_q4ngc1js5.s[7]++;
    while (currentIndex >= 0) {
      var parentTagName = (cov_q4ngc1js5.s[8]++, stack[currentIndex].tagName);
      cov_q4ngc1js5.s[9]++;
      if (parentTagName === tagName) {
        cov_q4ngc1js5.b[1][0]++;
        cov_q4ngc1js5.s[10]++;

        break;
      } else {
        cov_q4ngc1js5.b[1][1]++;
      }
      cov_q4ngc1js5.s[11]++;
      if ((0, _compat.arrayIncludes)(tagParents, parentTagName)) {
        cov_q4ngc1js5.b[2][0]++;
        cov_q4ngc1js5.s[12]++;

        return true;
      } else {
        cov_q4ngc1js5.b[2][1]++;
      }
      cov_q4ngc1js5.s[13]++;
      currentIndex--;
    }
  } else {
    cov_q4ngc1js5.b[0][1]++;
  }
  cov_q4ngc1js5.s[14]++;
  return false;
}

function rewindStack(stack, newLength, childrenEndPosition, endPosition) {
  cov_q4ngc1js5.f[2]++;
  cov_q4ngc1js5.s[15]++;

  stack[newLength].position.end = endPosition;
  cov_q4ngc1js5.s[16]++;
  for (var i = newLength + 1, len = stack.length; i < len; i++) {
    cov_q4ngc1js5.s[17]++;

    stack[i].position.end = childrenEndPosition;
  }
  cov_q4ngc1js5.s[18]++;
  stack.splice(newLength);
}

function parse(state) {
  cov_q4ngc1js5.f[3]++;

  var _ref = (cov_q4ngc1js5.s[19]++, state),
      tokens = _ref.tokens,
      options = _ref.options;

  var _ref2 = (cov_q4ngc1js5.s[20]++, state),
      stack = _ref2.stack;

  var nodes = (cov_q4ngc1js5.s[21]++, stack[stack.length - 1].children);
  var len = (cov_q4ngc1js5.s[22]++, tokens.length);

  var _ref3 = (cov_q4ngc1js5.s[23]++, state),
      cursor = _ref3.cursor;

  cov_q4ngc1js5.s[24]++;

  while (cursor < len) {
    var token = (cov_q4ngc1js5.s[25]++, tokens[cursor]);
    cov_q4ngc1js5.s[26]++;
    if (token.type !== 'tag-start') {
      cov_q4ngc1js5.b[3][0]++;
      cov_q4ngc1js5.s[27]++;

      nodes.push(token);
      cov_q4ngc1js5.s[28]++;
      cursor++;
      cov_q4ngc1js5.s[29]++;
      continue;
    } else {
      cov_q4ngc1js5.b[3][1]++;
    }

    var tagToken = (cov_q4ngc1js5.s[30]++, tokens[++cursor]);
    cov_q4ngc1js5.s[31]++;
    cursor++;
    var tagName = (cov_q4ngc1js5.s[32]++, tagToken.content.toLowerCase());
    cov_q4ngc1js5.s[33]++;
    if (token.close) {
      cov_q4ngc1js5.b[4][0]++;

      var index = (cov_q4ngc1js5.s[34]++, stack.length);
      var shouldRewind = (cov_q4ngc1js5.s[35]++, false);
      cov_q4ngc1js5.s[36]++;
      while (--index > -1) {
        cov_q4ngc1js5.s[37]++;

        if (stack[index].tagName === tagName) {
          cov_q4ngc1js5.b[5][0]++;
          cov_q4ngc1js5.s[38]++;

          shouldRewind = true;
          cov_q4ngc1js5.s[39]++;
          break;
        } else {
          cov_q4ngc1js5.b[5][1]++;
        }
      }
      cov_q4ngc1js5.s[40]++;
      while (cursor < len) {
        var endToken = (cov_q4ngc1js5.s[41]++, tokens[cursor]);
        cov_q4ngc1js5.s[42]++;
        if (endToken.type !== 'tag-end') {
            cov_q4ngc1js5.b[6][0]++;
            cov_q4ngc1js5.s[43]++;
            break;
          } else {
          cov_q4ngc1js5.b[6][1]++;
        }cov_q4ngc1js5.s[44]++;
        cursor++;
      }
      cov_q4ngc1js5.s[45]++;
      if (shouldRewind) {
        cov_q4ngc1js5.b[7][0]++;
        cov_q4ngc1js5.s[46]++;

        rewindStack(stack, index, token.position.start, tokens[cursor - 1].position.end);
        cov_q4ngc1js5.s[47]++;
        break;
      } else {
        cov_q4ngc1js5.b[7][1]++;
        cov_q4ngc1js5.s[48]++;

        continue;
      }
    } else {
      cov_q4ngc1js5.b[4][1]++;
    }

    var isClosingTag = (cov_q4ngc1js5.s[49]++, (0, _compat.arrayIncludes)(options.closingTags, tagName));
    var shouldRewindToAutoClose = (cov_q4ngc1js5.s[50]++, isClosingTag);
    cov_q4ngc1js5.s[51]++;
    if (shouldRewindToAutoClose) {
      cov_q4ngc1js5.b[8][0]++;

      var _ref4 = (cov_q4ngc1js5.s[52]++, options),
          terminals = _ref4.closingTagAncestorBreakers;

      cov_q4ngc1js5.s[53]++;

      shouldRewindToAutoClose = !hasTerminalParent(tagName, stack, terminals);
    } else {
      cov_q4ngc1js5.b[8][1]++;
    }

    cov_q4ngc1js5.s[54]++;
    if (shouldRewindToAutoClose) {
      cov_q4ngc1js5.b[9][0]++;

      // rewind the stack to just above the previous
      // closing tag of the same name
      var currentIndex = (cov_q4ngc1js5.s[55]++, stack.length - 1);
      cov_q4ngc1js5.s[56]++;
      while (currentIndex > 0) {
        cov_q4ngc1js5.s[57]++;

        if (tagName === stack[currentIndex].tagName) {
          cov_q4ngc1js5.b[10][0]++;
          cov_q4ngc1js5.s[58]++;

          rewindStack(stack, currentIndex, token.position.start, token.position.start);
          var previousIndex = (cov_q4ngc1js5.s[59]++, currentIndex - 1);
          cov_q4ngc1js5.s[60]++;
          nodes = stack[previousIndex].children;
          cov_q4ngc1js5.s[61]++;
          break;
        } else {
          cov_q4ngc1js5.b[10][1]++;
        }
        cov_q4ngc1js5.s[62]++;
        currentIndex = currentIndex - 1;
      }
    } else {
      cov_q4ngc1js5.b[9][1]++;
    }

    var attributes = (cov_q4ngc1js5.s[63]++, []);
    var attrToken = void 0;
    cov_q4ngc1js5.s[64]++;
    while (cursor < len) {
      cov_q4ngc1js5.s[65]++;

      attrToken = tokens[cursor];
      cov_q4ngc1js5.s[66]++;
      if (attrToken.type === 'tag-end') {
          cov_q4ngc1js5.b[11][0]++;
          cov_q4ngc1js5.s[67]++;
          break;
        } else {
        cov_q4ngc1js5.b[11][1]++;
      }cov_q4ngc1js5.s[68]++;
      attributes.push(attrToken.content);
      cov_q4ngc1js5.s[69]++;
      cursor++;
    }

    cov_q4ngc1js5.s[70]++;
    cursor++;
    var children = (cov_q4ngc1js5.s[71]++, []);
    var position = (cov_q4ngc1js5.s[72]++, {
      start: token.position.start,
      end: attrToken.position.end
    });
    var elementNode = (cov_q4ngc1js5.s[73]++, {
      type: 'element',
      tagName: tagToken.content,
      attributes: attributes,
      children: children,
      position: position
    });
    cov_q4ngc1js5.s[74]++;
    nodes.push(elementNode);

    var hasChildren = (cov_q4ngc1js5.s[75]++, !((cov_q4ngc1js5.b[12][0]++, attrToken.close) || (cov_q4ngc1js5.b[12][1]++, (0, _compat.arrayIncludes)(options.voidTags, tagName))));
    cov_q4ngc1js5.s[76]++;
    if (hasChildren) {
      cov_q4ngc1js5.b[13][0]++;

      var size = (cov_q4ngc1js5.s[77]++, stack.push({ tagName: tagName, children: children, position: position }));
      var innerState = (cov_q4ngc1js5.s[78]++, { tokens: tokens, options: options, cursor: cursor, stack: stack });
      cov_q4ngc1js5.s[79]++;
      parse(innerState);
      cov_q4ngc1js5.s[80]++;
      cursor = innerState.cursor;
      var rewoundInElement = (cov_q4ngc1js5.s[81]++, stack.length === size);
      cov_q4ngc1js5.s[82]++;
      if (rewoundInElement) {
        cov_q4ngc1js5.b[14][0]++;
        cov_q4ngc1js5.s[83]++;

        elementNode.position.end = tokens[cursor - 1].position.end;
      } else {
        cov_q4ngc1js5.b[14][1]++;
      }
    } else {
      cov_q4ngc1js5.b[13][1]++;
    }
  }
  cov_q4ngc1js5.s[84]++;
  state.cursor = cursor;
}

},{"./compat":1}],6:[function(require,module,exports){
'use strict';

var cov_fs4bzhlz4 = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/stringify.js',
      hash = '4a6a4628f3d12bd91f868fee07f716c74df89307',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/stringify.js',
    statementMap: {
      '0': {
        start: {
          line: 4,
          column: 2
        },
        end: {
          line: 12,
          column: 8
        }
      },
      '1': {
        start: {
          line: 5,
          column: 25
        },
        end: {
          line: 5,
          column: 34
        }
      },
      '2': {
        start: {
          line: 6,
          column: 4
        },
        end: {
          line: 8,
          column: 5
        }
      },
      '3': {
        start: {
          line: 7,
          column: 6
        },
        end: {
          line: 7,
          column: 30
        }
      },
      '4': {
        start: {
          line: 9,
          column: 24
        },
        end: {
          line: 9,
          column: 50
        }
      },
      '5': {
        start: {
          line: 10,
          column: 18
        },
        end: {
          line: 10,
          column: 42
        }
      },
      '6': {
        start: {
          line: 11,
          column: 4
        },
        end: {
          line: 11,
          column: 53
        }
      },
      '7': {
        start: {
          line: 16,
          column: 2
        },
        end: {
          line: 28,
          column: 13
        }
      },
      '8': {
        start: {
          line: 17,
          column: 4
        },
        end: {
          line: 19,
          column: 5
        }
      },
      '9': {
        start: {
          line: 18,
          column: 6
        },
        end: {
          line: 18,
          column: 25
        }
      },
      '10': {
        start: {
          line: 20,
          column: 4
        },
        end: {
          line: 22,
          column: 5
        }
      },
      '11': {
        start: {
          line: 21,
          column: 6
        },
        end: {
          line: 21,
          column: 37
        }
      },
      '12': {
        start: {
          line: 23,
          column: 44
        },
        end: {
          line: 23,
          column: 48
        }
      },
      '13': {
        start: {
          line: 24,
          column: 26
        },
        end: {
          line: 24,
          column: 80
        }
      },
      '14': {
        start: {
          line: 25,
          column: 4
        },
        end: {
          line: 27,
          column: 94
        }
      }
    },
    fnMap: {
      '0': {
        name: 'formatAttributes',
        decl: {
          start: {
            line: 3,
            column: 16
          },
          end: {
            line: 3,
            column: 32
          }
        },
        loc: {
          start: {
            line: 3,
            column: 46
          },
          end: {
            line: 13,
            column: 1
          }
        },
        line: 3
      },
      '1': {
        name: '(anonymous_1)',
        decl: {
          start: {
            line: 4,
            column: 27
          },
          end: {
            line: 4,
            column: 28
          }
        },
        loc: {
          start: {
            line: 4,
            column: 49
          },
          end: {
            line: 12,
            column: 3
          }
        },
        line: 4
      },
      '2': {
        name: 'toHTML',
        decl: {
          start: {
            line: 15,
            column: 16
          },
          end: {
            line: 15,
            column: 22
          }
        },
        loc: {
          start: {
            line: 15,
            column: 39
          },
          end: {
            line: 29,
            column: 1
          }
        },
        line: 15
      },
      '3': {
        name: '(anonymous_3)',
        decl: {
          start: {
            line: 16,
            column: 18
          },
          end: {
            line: 16,
            column: 19
          }
        },
        loc: {
          start: {
            line: 16,
            column: 26
          },
          end: {
            line: 28,
            column: 3
          }
        },
        line: 16
      }
    },
    branchMap: {
      '0': {
        loc: {
          start: {
            line: 6,
            column: 4
          },
          end: {
            line: 8,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 6,
            column: 4
          },
          end: {
            line: 8,
            column: 5
          }
        }, {
          start: {
            line: 6,
            column: 4
          },
          end: {
            line: 8,
            column: 5
          }
        }],
        line: 6
      },
      '1': {
        loc: {
          start: {
            line: 10,
            column: 18
          },
          end: {
            line: 10,
            column: 42
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 10,
            column: 32
          },
          end: {
            line: 10,
            column: 35
          }
        }, {
          start: {
            line: 10,
            column: 38
          },
          end: {
            line: 10,
            column: 42
          }
        }],
        line: 10
      },
      '2': {
        loc: {
          start: {
            line: 17,
            column: 4
          },
          end: {
            line: 19,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 17,
            column: 4
          },
          end: {
            line: 19,
            column: 5
          }
        }, {
          start: {
            line: 17,
            column: 4
          },
          end: {
            line: 19,
            column: 5
          }
        }],
        line: 17
      },
      '3': {
        loc: {
          start: {
            line: 20,
            column: 4
          },
          end: {
            line: 22,
            column: 5
          }
        },
        type: 'if',
        locations: [{
          start: {
            line: 20,
            column: 4
          },
          end: {
            line: 22,
            column: 5
          }
        }, {
          start: {
            line: 20,
            column: 4
          },
          end: {
            line: 22,
            column: 5
          }
        }],
        line: 20
      },
      '4': {
        loc: {
          start: {
            line: 25,
            column: 11
          },
          end: {
            line: 27,
            column: 94
          }
        },
        type: 'cond-expr',
        locations: [{
          start: {
            line: 26,
            column: 8
          },
          end: {
            line: 26,
            column: 53
          }
        }, {
          start: {
            line: 27,
            column: 8
          },
          end: {
            line: 27,
            column: 94
          }
        }],
        line: 25
      }
    },
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0,
      '4': 0,
      '5': 0,
      '6': 0,
      '7': 0,
      '8': 0,
      '9': 0,
      '10': 0,
      '11': 0,
      '12': 0,
      '13': 0,
      '14': 0
    },
    f: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0
    },
    b: {
      '0': [0, 0],
      '1': [0, 0],
      '2': [0, 0],
      '3': [0, 0],
      '4': [0, 0]
    },
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.formatAttributes = formatAttributes;
exports.toHTML = toHTML;

var _compat = require('./compat');

function formatAttributes(attributes) {
  cov_fs4bzhlz4.f[0]++;
  cov_fs4bzhlz4.s[0]++;

  return attributes.reduce(function (attrs, attribute) {
    cov_fs4bzhlz4.f[1]++;

    var _ref = (cov_fs4bzhlz4.s[1]++, attribute),
        key = _ref.key,
        value = _ref.value;

    cov_fs4bzhlz4.s[2]++;

    if (value === null) {
      cov_fs4bzhlz4.b[0][0]++;
      cov_fs4bzhlz4.s[3]++;

      return attrs + ' ' + key;
    } else {
      cov_fs4bzhlz4.b[0][1]++;
    }
    var quoteEscape = (cov_fs4bzhlz4.s[4]++, value.indexOf('\'') !== -1);
    var quote = (cov_fs4bzhlz4.s[5]++, quoteEscape ? (cov_fs4bzhlz4.b[1][0]++, '"') : (cov_fs4bzhlz4.b[1][1]++, '\''));
    cov_fs4bzhlz4.s[6]++;
    return attrs + ' ' + key + '=' + quote + value + quote;
  }, '');
}

function toHTML(tree, options) {
  cov_fs4bzhlz4.f[2]++;
  cov_fs4bzhlz4.s[7]++;

  return tree.map(function (node) {
    cov_fs4bzhlz4.f[3]++;
    cov_fs4bzhlz4.s[8]++;

    if (node.type === 'text') {
      cov_fs4bzhlz4.b[2][0]++;
      cov_fs4bzhlz4.s[9]++;

      return node.content;
    } else {
      cov_fs4bzhlz4.b[2][1]++;
    }
    cov_fs4bzhlz4.s[10]++;
    if (node.type === 'comment') {
      cov_fs4bzhlz4.b[3][0]++;
      cov_fs4bzhlz4.s[11]++;

      return '<!--' + node.content + '-->';
    } else {
      cov_fs4bzhlz4.b[3][1]++;
    }

    var _ref2 = (cov_fs4bzhlz4.s[12]++, node),
        tagName = _ref2.tagName,
        attributes = _ref2.attributes,
        children = _ref2.children;

    var isSelfClosing = (cov_fs4bzhlz4.s[13]++, (0, _compat.arrayIncludes)(options.voidTags, tagName.toLowerCase()));
    cov_fs4bzhlz4.s[14]++;
    return isSelfClosing ? (cov_fs4bzhlz4.b[4][0]++, '<' + tagName + formatAttributes(attributes) + '>') : (cov_fs4bzhlz4.b[4][1]++, '<' + tagName + formatAttributes(attributes) + '>' + toHTML(children, options) + '</' + tagName + '>');
  }).join('');
}

exports.default = { toHTML: toHTML };

},{"./compat":1}],7:[function(require,module,exports){
'use strict';

var cov_ebkruvd2n = function () {
  var path = '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/tags.js',
      hash = '6039b9f65d15797c952509955976acf6930e65a4',
      Function = function () {}.constructor,
      global = new Function('return this')(),
      gcv = '__coverage__',
      coverageData = {
    path: '/Users/chrisandrejewski/Desktop/Work/github-repos/himalaya/src/tags.js',
    statementMap: {
      '0': {
        start: {
          line: 5,
          column: 29
        },
        end: {
          line: 5,
          column: 60
        }
      },
      '1': {
        start: {
          line: 11,
          column: 27
        },
        end: {
          line: 14,
          column: 1
        }
      },
      '2': {
        start: {
          line: 23,
          column: 42
        },
        end: {
          line: 32,
          column: 1
        }
      },
      '3': {
        start: {
          line: 38,
          column: 24
        },
        end: {
          line: 42,
          column: 1
        }
      }
    },
    fnMap: {},
    branchMap: {},
    s: {
      '0': 0,
      '1': 0,
      '2': 0,
      '3': 0
    },
    f: {},
    b: {},
    _coverageSchema: '332fd63041d2c1bcb487cc26dd0d5f7d97098a6c'
  },
      coverage = global[gcv] || (global[gcv] = {});

  if (coverage[path] && coverage[path].hash === hash) {
    return coverage[path];
  }

  coverageData.hash = hash;
  return coverage[path] = coverageData;
}();

Object.defineProperty(exports, "__esModule", {
  value: true
});
/*
  Tags which contain arbitary non-parsed content
  For example: <script> JavaScript should not be parsed
*/
var childlessTags = exports.childlessTags = (cov_ebkruvd2n.s[0]++, ['style', 'script', 'template']);

/*
  Tags which auto-close because they cannot be nested
  For example: <p>Outer<p>Inner is <p>Outer</p><p>Inner</p>
*/
var closingTags = exports.closingTags = (cov_ebkruvd2n.s[1]++, ['html', 'head', 'body', 'p', 'dt', 'dd', 'li', 'option', 'thead', 'th', 'tbody', 'tr', 'td', 'tfoot', 'colgroup']);

/*
  Closing tags which have ancestor tags which
  may exist within them which prevent the
  closing tag from auto-closing.
  For example: in <li><ul><li></ul></li>,
  the top-level <li> should not auto-close.
*/
var closingTagAncestorBreakers = exports.closingTagAncestorBreakers = (cov_ebkruvd2n.s[2]++, {
  li: ['ul', 'ol', 'menu'],
  dt: ['dl'],
  dd: ['dl'],
  tbody: ['table'],
  thead: ['table'],
  tfoot: ['table'],
  tr: ['table'],
  td: ['table']

  /*
    Tags which do not need the closing tag
    For example: <img> does not need </img>
  */
});var voidTags = exports.voidTags = (cov_ebkruvd2n.s[3]++, ['!doctype', 'area', 'base', 'br', 'col', 'command', 'embed', 'hr', 'img', 'input', 'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr']);

},{}]},{},[3])(3)
});
/*! highlight.js v9.12.0 | BSD3 License | git.io/hljslicense */
!function(e){var n="object"==typeof window&&window||"object"==typeof self&&self;"undefined"!=typeof exports?e(exports):n&&(n.hljs=e({}),"function"==typeof define&&define.amd&&define([],function(){return n.hljs}))}(function(e){function n(e){return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")}function t(e){return e.nodeName.toLowerCase()}function r(e,n){var t=e&&e.exec(n);return t&&0===t.index}function a(e){return k.test(e)}function i(e){var n,t,r,i,o=e.className+" ";if(o+=e.parentNode?e.parentNode.className:"",t=B.exec(o))return w(t[1])?t[1]:"no-highlight";for(o=o.split(/\s+/),n=0,r=o.length;r>n;n++)if(i=o[n],a(i)||w(i))return i}function o(e){var n,t={},r=Array.prototype.slice.call(arguments,1);for(n in e)t[n]=e[n];return r.forEach(function(e){for(n in e)t[n]=e[n]}),t}function u(e){var n=[];return function r(e,a){for(var i=e.firstChild;i;i=i.nextSibling)3===i.nodeType?a+=i.nodeValue.length:1===i.nodeType&&(n.push({event:"start",offset:a,node:i}),a=r(i,a),t(i).match(/br|hr|img|input/)||n.push({event:"stop",offset:a,node:i}));return a}(e,0),n}function c(e,r,a){function i(){return e.length&&r.length?e[0].offset!==r[0].offset?e[0].offset<r[0].offset?e:r:"start"===r[0].event?e:r:e.length?e:r}function o(e){function r(e){return" "+e.nodeName+'="'+n(e.value).replace('"',"&quot;")+'"'}s+="<"+t(e)+E.map.call(e.attributes,r).join("")+">"}function u(e){s+="</"+t(e)+">"}function c(e){("start"===e.event?o:u)(e.node)}for(var l=0,s="",f=[];e.length||r.length;){var g=i();if(s+=n(a.substring(l,g[0].offset)),l=g[0].offset,g===e){f.reverse().forEach(u);do c(g.splice(0,1)[0]),g=i();while(g===e&&g.length&&g[0].offset===l);f.reverse().forEach(o)}else"start"===g[0].event?f.push(g[0].node):f.pop(),c(g.splice(0,1)[0])}return s+n(a.substr(l))}function l(e){return e.v&&!e.cached_variants&&(e.cached_variants=e.v.map(function(n){return o(e,{v:null},n)})),e.cached_variants||e.eW&&[o(e)]||[e]}function s(e){function n(e){return e&&e.source||e}function t(t,r){return new RegExp(n(t),"m"+(e.cI?"i":"")+(r?"g":""))}function r(a,i){if(!a.compiled){if(a.compiled=!0,a.k=a.k||a.bK,a.k){var o={},u=function(n,t){e.cI&&(t=t.toLowerCase()),t.split(" ").forEach(function(e){var t=e.split("|");o[t[0]]=[n,t[1]?Number(t[1]):1]})};"string"==typeof a.k?u("keyword",a.k):x(a.k).forEach(function(e){u(e,a.k[e])}),a.k=o}a.lR=t(a.l||/\w+/,!0),i&&(a.bK&&(a.b="\\b("+a.bK.split(" ").join("|")+")\\b"),a.b||(a.b=/\B|\b/),a.bR=t(a.b),a.e||a.eW||(a.e=/\B|\b/),a.e&&(a.eR=t(a.e)),a.tE=n(a.e)||"",a.eW&&i.tE&&(a.tE+=(a.e?"|":"")+i.tE)),a.i&&(a.iR=t(a.i)),null==a.r&&(a.r=1),a.c||(a.c=[]),a.c=Array.prototype.concat.apply([],a.c.map(function(e){return l("self"===e?a:e)})),a.c.forEach(function(e){r(e,a)}),a.starts&&r(a.starts,i);var c=a.c.map(function(e){return e.bK?"\\.?("+e.b+")\\.?":e.b}).concat([a.tE,a.i]).map(n).filter(Boolean);a.t=c.length?t(c.join("|"),!0):{exec:function(){return null}}}}r(e)}function f(e,t,a,i){function o(e,n){var t,a;for(t=0,a=n.c.length;a>t;t++)if(r(n.c[t].bR,e))return n.c[t]}function u(e,n){if(r(e.eR,n)){for(;e.endsParent&&e.parent;)e=e.parent;return e}return e.eW?u(e.parent,n):void 0}function c(e,n){return!a&&r(n.iR,e)}function l(e,n){var t=N.cI?n[0].toLowerCase():n[0];return e.k.hasOwnProperty(t)&&e.k[t]}function p(e,n,t,r){var a=r?"":I.classPrefix,i='<span class="'+a,o=t?"":C;return i+=e+'">',i+n+o}function h(){var e,t,r,a;if(!E.k)return n(k);for(a="",t=0,E.lR.lastIndex=0,r=E.lR.exec(k);r;)a+=n(k.substring(t,r.index)),e=l(E,r),e?(B+=e[1],a+=p(e[0],n(r[0]))):a+=n(r[0]),t=E.lR.lastIndex,r=E.lR.exec(k);return a+n(k.substr(t))}function d(){var e="string"==typeof E.sL;if(e&&!y[E.sL])return n(k);var t=e?f(E.sL,k,!0,x[E.sL]):g(k,E.sL.length?E.sL:void 0);return E.r>0&&(B+=t.r),e&&(x[E.sL]=t.top),p(t.language,t.value,!1,!0)}function b(){L+=null!=E.sL?d():h(),k=""}function v(e){L+=e.cN?p(e.cN,"",!0):"",E=Object.create(e,{parent:{value:E}})}function m(e,n){if(k+=e,null==n)return b(),0;var t=o(n,E);if(t)return t.skip?k+=n:(t.eB&&(k+=n),b(),t.rB||t.eB||(k=n)),v(t,n),t.rB?0:n.length;var r=u(E,n);if(r){var a=E;a.skip?k+=n:(a.rE||a.eE||(k+=n),b(),a.eE&&(k=n));do E.cN&&(L+=C),E.skip||(B+=E.r),E=E.parent;while(E!==r.parent);return r.starts&&v(r.starts,""),a.rE?0:n.length}if(c(n,E))throw new Error('Illegal lexeme "'+n+'" for mode "'+(E.cN||"<unnamed>")+'"');return k+=n,n.length||1}var N=w(e);if(!N)throw new Error('Unknown language: "'+e+'"');s(N);var R,E=i||N,x={},L="";for(R=E;R!==N;R=R.parent)R.cN&&(L=p(R.cN,"",!0)+L);var k="",B=0;try{for(var M,j,O=0;;){if(E.t.lastIndex=O,M=E.t.exec(t),!M)break;j=m(t.substring(O,M.index),M[0]),O=M.index+j}for(m(t.substr(O)),R=E;R.parent;R=R.parent)R.cN&&(L+=C);return{r:B,value:L,language:e,top:E}}catch(T){if(T.message&&-1!==T.message.indexOf("Illegal"))return{r:0,value:n(t)};throw T}}function g(e,t){t=t||I.languages||x(y);var r={r:0,value:n(e)},a=r;return t.filter(w).forEach(function(n){var t=f(n,e,!1);t.language=n,t.r>a.r&&(a=t),t.r>r.r&&(a=r,r=t)}),a.language&&(r.second_best=a),r}function p(e){return I.tabReplace||I.useBR?e.replace(M,function(e,n){return I.useBR&&"\n"===e?"<br>":I.tabReplace?n.replace(/\t/g,I.tabReplace):""}):e}function h(e,n,t){var r=n?L[n]:t,a=[e.trim()];return e.match(/\bhljs\b/)||a.push("hljs"),-1===e.indexOf(r)&&a.push(r),a.join(" ").trim()}function d(e){var n,t,r,o,l,s=i(e);a(s)||(I.useBR?(n=document.createElementNS("http://www.w3.org/1999/xhtml","div"),n.innerHTML=e.innerHTML.replace(/\n/g,"").replace(/<br[ \/]*>/g,"\n")):n=e,l=n.textContent,r=s?f(s,l,!0):g(l),t=u(n),t.length&&(o=document.createElementNS("http://www.w3.org/1999/xhtml","div"),o.innerHTML=r.value,r.value=c(t,u(o),l)),r.value=p(r.value),e.innerHTML=r.value,e.className=h(e.className,s,r.language),e.result={language:r.language,re:r.r},r.second_best&&(e.second_best={language:r.second_best.language,re:r.second_best.r}))}function b(e){I=o(I,e)}function v(){if(!v.called){v.called=!0;var e=document.querySelectorAll("pre code");E.forEach.call(e,d)}}function m(){addEventListener("DOMContentLoaded",v,!1),addEventListener("load",v,!1)}function N(n,t){var r=y[n]=t(e);r.aliases&&r.aliases.forEach(function(e){L[e]=n})}function R(){return x(y)}function w(e){return e=(e||"").toLowerCase(),y[e]||y[L[e]]}var E=[],x=Object.keys,y={},L={},k=/^(no-?highlight|plain|text)$/i,B=/\blang(?:uage)?-([\w-]+)\b/i,M=/((^(<[^>]+>|\t|)+|(?:\n)))/gm,C="</span>",I={classPrefix:"hljs-",tabReplace:null,useBR:!1,languages:void 0};return e.highlight=f,e.highlightAuto=g,e.fixMarkup=p,e.highlightBlock=d,e.configure=b,e.initHighlighting=v,e.initHighlightingOnLoad=m,e.registerLanguage=N,e.listLanguages=R,e.getLanguage=w,e.inherit=o,e.IR="[a-zA-Z]\\w*",e.UIR="[a-zA-Z_]\\w*",e.NR="\\b\\d+(\\.\\d+)?",e.CNR="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",e.BNR="\\b(0b[01]+)",e.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",e.BE={b:"\\\\[\\s\\S]",r:0},e.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[e.BE]},e.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[e.BE]},e.PWM={b:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/},e.C=function(n,t,r){var a=e.inherit({cN:"comment",b:n,e:t,c:[]},r||{});return a.c.push(e.PWM),a.c.push({cN:"doctag",b:"(?:TODO|FIXME|NOTE|BUG|XXX):",r:0}),a},e.CLCM=e.C("//","$"),e.CBCM=e.C("/\\*","\\*/"),e.HCM=e.C("#","$"),e.NM={cN:"number",b:e.NR,r:0},e.CNM={cN:"number",b:e.CNR,r:0},e.BNM={cN:"number",b:e.BNR,r:0},e.CSSNM={cN:"number",b:e.NR+"(%|em|ex|ch|rem|vw|vh|vmin|vmax|cm|mm|in|pt|pc|px|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx)?",r:0},e.RM={cN:"regexp",b:/\//,e:/\/[gimuy]*/,i:/\n/,c:[e.BE,{b:/\[/,e:/\]/,r:0,c:[e.BE]}]},e.TM={cN:"title",b:e.IR,r:0},e.UTM={cN:"title",b:e.UIR,r:0},e.METHOD_GUARD={b:"\\.\\s*"+e.UIR,r:0},e});hljs.registerLanguage("javascript",function(e){var r="[A-Za-z$_][0-9A-Za-z$_]*",t={keyword:"in of if for while finally var new function do return void else break catch instanceof with throw case default try this switch continue typeof delete let yield const export super debugger as async await static import from as",literal:"true false null undefined NaN Infinity",built_in:"eval isFinite isNaN parseFloat parseInt decodeURI decodeURIComponent encodeURI encodeURIComponent escape unescape Object Function Boolean Error EvalError InternalError RangeError ReferenceError StopIteration SyntaxError TypeError URIError Number Math Date String RegExp Array Float32Array Float64Array Int16Array Int32Array Int8Array Uint16Array Uint32Array Uint8Array Uint8ClampedArray ArrayBuffer DataView JSON Intl arguments require module console window document Symbol Set Map WeakSet WeakMap Proxy Reflect Promise"},a={cN:"number",v:[{b:"\\b(0[bB][01]+)"},{b:"\\b(0[oO][0-7]+)"},{b:e.CNR}],r:0},n={cN:"subst",b:"\\$\\{",e:"\\}",k:t,c:[]},c={cN:"string",b:"`",e:"`",c:[e.BE,n]};n.c=[e.ASM,e.QSM,c,a,e.RM];var s=n.c.concat([e.CBCM,e.CLCM]);return{aliases:["js","jsx"],k:t,c:[{cN:"meta",r:10,b:/^\s*['"]use (strict|asm)['"]/},{cN:"meta",b:/^#!/,e:/$/},e.ASM,e.QSM,c,e.CLCM,e.CBCM,a,{b:/[{,]\s*/,r:0,c:[{b:r+"\\s*:",rB:!0,r:0,c:[{cN:"attr",b:r,r:0}]}]},{b:"("+e.RSR+"|\\b(case|return|throw)\\b)\\s*",k:"return throw case",c:[e.CLCM,e.CBCM,e.RM,{cN:"function",b:"(\\(.*?\\)|"+r+")\\s*=>",rB:!0,e:"\\s*=>",c:[{cN:"params",v:[{b:r},{b:/\(\s*\)/},{b:/\(/,e:/\)/,eB:!0,eE:!0,k:t,c:s}]}]},{b:/</,e:/(\/\w+|\w+\/)>/,sL:"xml",c:[{b:/<\w+\s*\/>/,skip:!0},{b:/<\w+/,e:/(\/\w+|\w+\/)>/,skip:!0,c:[{b:/<\w+\s*\/>/,skip:!0},"self"]}]}],r:0},{cN:"function",bK:"function",e:/\{/,eE:!0,c:[e.inherit(e.TM,{b:r}),{cN:"params",b:/\(/,e:/\)/,eB:!0,eE:!0,c:s}],i:/\[|%/},{b:/\$[(.]/},e.METHOD_GUARD,{cN:"class",bK:"class",e:/[{;=]/,eE:!0,i:/[:"\[\]]/,c:[{bK:"extends"},e.UTM]},{bK:"constructor",e:/\{/,eE:!0}],i:/#(?!!)/}});hljs.registerLanguage("bash",function(e){var t={cN:"variable",v:[{b:/\$[\w\d#@][\w\d_]*/},{b:/\$\{(.*?)}/}]},s={cN:"string",b:/"/,e:/"/,c:[e.BE,t,{cN:"variable",b:/\$\(/,e:/\)/,c:[e.BE]}]},a={cN:"string",b:/'/,e:/'/};return{aliases:["sh","zsh"],l:/\b-?[a-z\._]+\b/,k:{keyword:"if then else elif fi for while in do done case esac function",literal:"true false",built_in:"break cd continue eval exec exit export getopts hash pwd readonly return shift test times trap umask unset alias bind builtin caller command declare echo enable help let local logout mapfile printf read readarray source type typeset ulimit unalias set shopt autoload bg bindkey bye cap chdir clone comparguments compcall compctl compdescribe compfiles compgroups compquote comptags comptry compvalues dirs disable disown echotc echoti emulate fc fg float functions getcap getln history integer jobs kill limit log noglob popd print pushd pushln rehash sched setcap setopt stat suspend ttyctl unfunction unhash unlimit unsetopt vared wait whence where which zcompile zformat zftp zle zmodload zparseopts zprof zpty zregexparse zsocket zstyle ztcp",_:"-ne -eq -lt -gt -f -d -e -s -l -a"},c:[{cN:"meta",b:/^#![^\n]+sh\s*$/,r:10},{cN:"function",b:/\w[\w\d_]*\s*\(\s*\)\s*\{/,rB:!0,c:[e.inherit(e.TM,{b:/\w[\w\d_]*/})],r:0},e.HCM,s,a,t]}});hljs.registerLanguage("xml",function(s){var e="[A-Za-z0-9\\._:-]+",t={eW:!0,i:/</,r:0,c:[{cN:"attr",b:e,r:0},{b:/=\s*/,r:0,c:[{cN:"string",endsParent:!0,v:[{b:/"/,e:/"/},{b:/'/,e:/'/},{b:/[^\s"'=<>`]+/}]}]}]};return{aliases:["html","xhtml","rss","atom","xjb","xsd","xsl","plist"],cI:!0,c:[{cN:"meta",b:"<!DOCTYPE",e:">",r:10,c:[{b:"\\[",e:"\\]"}]},s.C("<!--","-->",{r:10}),{b:"<\\!\\[CDATA\\[",e:"\\]\\]>",r:10},{b:/<\?(php)?/,e:/\?>/,sL:"php",c:[{b:"/\\*",e:"\\*/",skip:!0}]},{cN:"tag",b:"<style(?=\\s|>|$)",e:">",k:{name:"style"},c:[t],starts:{e:"</style>",rE:!0,sL:["css","xml"]}},{cN:"tag",b:"<script(?=\\s|>|$)",e:">",k:{name:"script"},c:[t],starts:{e:"</script>",rE:!0,sL:["actionscript","javascript","handlebars","xml"]}},{cN:"meta",v:[{b:/<\?xml/,e:/\?>/,r:10},{b:/<\?\w+/,e:/\?>/}]},{cN:"tag",b:"</?",e:"/?>",c:[{cN:"name",b:/[^\/><\s]+/,r:0},t]}]}});hljs.registerLanguage("json",function(e){var i={literal:"true false null"},n=[e.QSM,e.CNM],r={e:",",eW:!0,eE:!0,c:n,k:i},t={b:"{",e:"}",c:[{cN:"attr",b:/"/,e:/"/,c:[e.BE],i:"\\n"},e.inherit(r,{b:/:/})],i:"\\S"},c={b:"\\[",e:"\\]",c:[e.inherit(r)],i:"\\S"};return n.splice(n.length,0,t,c),{c:n,k:i,i:"\\S"}});
!function(e,t){"object"==typeof exports&&"undefined"!=typeof module?module.exports=t():"function"==typeof define&&define.amd?define(t):e.moment=t()}(this,function(){"use strict";var e,i;function c(){return e.apply(null,arguments)}function o(e){return e instanceof Array||"[object Array]"===Object.prototype.toString.call(e)}function u(e){return null!=e&&"[object Object]"===Object.prototype.toString.call(e)}function l(e){return void 0===e}function d(e){return"number"==typeof e||"[object Number]"===Object.prototype.toString.call(e)}function h(e){return e instanceof Date||"[object Date]"===Object.prototype.toString.call(e)}function f(e,t){var n,s=[];for(n=0;n<e.length;++n)s.push(t(e[n],n));return s}function m(e,t){return Object.prototype.hasOwnProperty.call(e,t)}function _(e,t){for(var n in t)m(t,n)&&(e[n]=t[n]);return m(t,"toString")&&(e.toString=t.toString),m(t,"valueOf")&&(e.valueOf=t.valueOf),e}function y(e,t,n,s){return Ot(e,t,n,s,!0).utc()}function g(e){return null==e._pf&&(e._pf={empty:!1,unusedTokens:[],unusedInput:[],overflow:-2,charsLeftOver:0,nullInput:!1,invalidMonth:null,invalidFormat:!1,userInvalidated:!1,iso:!1,parsedDateParts:[],meridiem:null,rfc2822:!1,weekdayMismatch:!1}),e._pf}function p(e){if(null==e._isValid){var t=g(e),n=i.call(t.parsedDateParts,function(e){return null!=e}),s=!isNaN(e._d.getTime())&&t.overflow<0&&!t.empty&&!t.invalidMonth&&!t.invalidWeekday&&!t.weekdayMismatch&&!t.nullInput&&!t.invalidFormat&&!t.userInvalidated&&(!t.meridiem||t.meridiem&&n);if(e._strict&&(s=s&&0===t.charsLeftOver&&0===t.unusedTokens.length&&void 0===t.bigHour),null!=Object.isFrozen&&Object.isFrozen(e))return s;e._isValid=s}return e._isValid}function v(e){var t=y(NaN);return null!=e?_(g(t),e):g(t).userInvalidated=!0,t}i=Array.prototype.some?Array.prototype.some:function(e){for(var t=Object(this),n=t.length>>>0,s=0;s<n;s++)if(s in t&&e.call(this,t[s],s,t))return!0;return!1};var r=c.momentProperties=[];function w(e,t){var n,s,i;if(l(t._isAMomentObject)||(e._isAMomentObject=t._isAMomentObject),l(t._i)||(e._i=t._i),l(t._f)||(e._f=t._f),l(t._l)||(e._l=t._l),l(t._strict)||(e._strict=t._strict),l(t._tzm)||(e._tzm=t._tzm),l(t._isUTC)||(e._isUTC=t._isUTC),l(t._offset)||(e._offset=t._offset),l(t._pf)||(e._pf=g(t)),l(t._locale)||(e._locale=t._locale),0<r.length)for(n=0;n<r.length;n++)l(i=t[s=r[n]])||(e[s]=i);return e}var t=!1;function M(e){w(this,e),this._d=new Date(null!=e._d?e._d.getTime():NaN),this.isValid()||(this._d=new Date(NaN)),!1===t&&(t=!0,c.updateOffset(this),t=!1)}function S(e){return e instanceof M||null!=e&&null!=e._isAMomentObject}function D(e){return e<0?Math.ceil(e)||0:Math.floor(e)}function k(e){var t=+e,n=0;return 0!==t&&isFinite(t)&&(n=D(t)),n}function a(e,t,n){var s,i=Math.min(e.length,t.length),r=Math.abs(e.length-t.length),a=0;for(s=0;s<i;s++)(n&&e[s]!==t[s]||!n&&k(e[s])!==k(t[s]))&&a++;return a+r}function Y(e){!1===c.suppressDeprecationWarnings&&"undefined"!=typeof console&&console.warn&&console.warn("Deprecation warning: "+e)}function n(i,r){var a=!0;return _(function(){if(null!=c.deprecationHandler&&c.deprecationHandler(null,i),a){for(var e,t=[],n=0;n<arguments.length;n++){if(e="","object"==typeof arguments[n]){for(var s in e+="\n["+n+"] ",arguments[0])e+=s+": "+arguments[0][s]+", ";e=e.slice(0,-2)}else e=arguments[n];t.push(e)}Y(i+"\nArguments: "+Array.prototype.slice.call(t).join("")+"\n"+(new Error).stack),a=!1}return r.apply(this,arguments)},r)}var s,O={};function T(e,t){null!=c.deprecationHandler&&c.deprecationHandler(e,t),O[e]||(Y(t),O[e]=!0)}function x(e){return e instanceof Function||"[object Function]"===Object.prototype.toString.call(e)}function b(e,t){var n,s=_({},e);for(n in t)m(t,n)&&(u(e[n])&&u(t[n])?(s[n]={},_(s[n],e[n]),_(s[n],t[n])):null!=t[n]?s[n]=t[n]:delete s[n]);for(n in e)m(e,n)&&!m(t,n)&&u(e[n])&&(s[n]=_({},s[n]));return s}function P(e){null!=e&&this.set(e)}c.suppressDeprecationWarnings=!1,c.deprecationHandler=null,s=Object.keys?Object.keys:function(e){var t,n=[];for(t in e)m(e,t)&&n.push(t);return n};var W={};function H(e,t){var n=e.toLowerCase();W[n]=W[n+"s"]=W[t]=e}function R(e){return"string"==typeof e?W[e]||W[e.toLowerCase()]:void 0}function C(e){var t,n,s={};for(n in e)m(e,n)&&(t=R(n))&&(s[t]=e[n]);return s}var F={};function L(e,t){F[e]=t}function U(e,t,n){var s=""+Math.abs(e),i=t-s.length;return(0<=e?n?"+":"":"-")+Math.pow(10,Math.max(0,i)).toString().substr(1)+s}var N=/(\[[^\[]*\])|(\\)?([Hh]mm(ss)?|Mo|MM?M?M?|Do|DDDo|DD?D?D?|ddd?d?|do?|w[o|w]?|W[o|W]?|Qo?|YYYYYY|YYYYY|YYYY|YY|gg(ggg?)?|GG(GGG?)?|e|E|a|A|hh?|HH?|kk?|mm?|ss?|S{1,9}|x|X|zz?|ZZ?|.)/g,G=/(\[[^\[]*\])|(\\)?(LTS|LT|LL?L?L?|l{1,4})/g,V={},E={};function I(e,t,n,s){var i=s;"string"==typeof s&&(i=function(){return this[s]()}),e&&(E[e]=i),t&&(E[t[0]]=function(){return U(i.apply(this,arguments),t[1],t[2])}),n&&(E[n]=function(){return this.localeData().ordinal(i.apply(this,arguments),e)})}function A(e,t){return e.isValid()?(t=j(t,e.localeData()),V[t]=V[t]||function(s){var e,i,t,r=s.match(N);for(e=0,i=r.length;e<i;e++)E[r[e]]?r[e]=E[r[e]]:r[e]=(t=r[e]).match(/\[[\s\S]/)?t.replace(/^\[|\]$/g,""):t.replace(/\\/g,"");return function(e){var t,n="";for(t=0;t<i;t++)n+=x(r[t])?r[t].call(e,s):r[t];return n}}(t),V[t](e)):e.localeData().invalidDate()}function j(e,t){var n=5;function s(e){return t.longDateFormat(e)||e}for(G.lastIndex=0;0<=n&&G.test(e);)e=e.replace(G,s),G.lastIndex=0,n-=1;return e}var Z=/\d/,z=/\d\d/,$=/\d{3}/,q=/\d{4}/,J=/[+-]?\d{6}/,B=/\d\d?/,Q=/\d\d\d\d?/,X=/\d\d\d\d\d\d?/,K=/\d{1,3}/,ee=/\d{1,4}/,te=/[+-]?\d{1,6}/,ne=/\d+/,se=/[+-]?\d+/,ie=/Z|[+-]\d\d:?\d\d/gi,re=/Z|[+-]\d\d(?::?\d\d)?/gi,ae=/[0-9]{0,256}['a-z\u00A0-\u05FF\u0700-\uD7FF\uF900-\uFDCF\uFDF0-\uFF07\uFF10-\uFFEF]{1,256}|[\u0600-\u06FF\/]{1,256}(\s*?[\u0600-\u06FF]{1,256}){1,2}/i,oe={};function ue(e,n,s){oe[e]=x(n)?n:function(e,t){return e&&s?s:n}}function le(e,t){return m(oe,e)?oe[e](t._strict,t._locale):new RegExp(de(e.replace("\\","").replace(/\\(\[)|\\(\])|\[([^\]\[]*)\]|\\(.)/g,function(e,t,n,s,i){return t||n||s||i})))}function de(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var he={};function ce(e,n){var t,s=n;for("string"==typeof e&&(e=[e]),d(n)&&(s=function(e,t){t[n]=k(e)}),t=0;t<e.length;t++)he[e[t]]=s}function fe(e,i){ce(e,function(e,t,n,s){n._w=n._w||{},i(e,n._w,n,s)})}var me=0,_e=1,ye=2,ge=3,pe=4,ve=5,we=6,Me=7,Se=8;function De(e){return ke(e)?366:365}function ke(e){return e%4==0&&e%100!=0||e%400==0}I("Y",0,0,function(){var e=this.year();return e<=9999?""+e:"+"+e}),I(0,["YY",2],0,function(){return this.year()%100}),I(0,["YYYY",4],0,"year"),I(0,["YYYYY",5],0,"year"),I(0,["YYYYYY",6,!0],0,"year"),H("year","y"),L("year",1),ue("Y",se),ue("YY",B,z),ue("YYYY",ee,q),ue("YYYYY",te,J),ue("YYYYYY",te,J),ce(["YYYYY","YYYYYY"],me),ce("YYYY",function(e,t){t[me]=2===e.length?c.parseTwoDigitYear(e):k(e)}),ce("YY",function(e,t){t[me]=c.parseTwoDigitYear(e)}),ce("Y",function(e,t){t[me]=parseInt(e,10)}),c.parseTwoDigitYear=function(e){return k(e)+(68<k(e)?1900:2e3)};var Ye,Oe=Te("FullYear",!0);function Te(t,n){return function(e){return null!=e?(be(this,t,e),c.updateOffset(this,n),this):xe(this,t)}}function xe(e,t){return e.isValid()?e._d["get"+(e._isUTC?"UTC":"")+t]():NaN}function be(e,t,n){e.isValid()&&!isNaN(n)&&("FullYear"===t&&ke(e.year())&&1===e.month()&&29===e.date()?e._d["set"+(e._isUTC?"UTC":"")+t](n,e.month(),Pe(n,e.month())):e._d["set"+(e._isUTC?"UTC":"")+t](n))}function Pe(e,t){if(isNaN(e)||isNaN(t))return NaN;var n,s=(t%(n=12)+n)%n;return e+=(t-s)/12,1===s?ke(e)?29:28:31-s%7%2}Ye=Array.prototype.indexOf?Array.prototype.indexOf:function(e){var t;for(t=0;t<this.length;++t)if(this[t]===e)return t;return-1},I("M",["MM",2],"Mo",function(){return this.month()+1}),I("MMM",0,0,function(e){return this.localeData().monthsShort(this,e)}),I("MMMM",0,0,function(e){return this.localeData().months(this,e)}),H("month","M"),L("month",8),ue("M",B),ue("MM",B,z),ue("MMM",function(e,t){return t.monthsShortRegex(e)}),ue("MMMM",function(e,t){return t.monthsRegex(e)}),ce(["M","MM"],function(e,t){t[_e]=k(e)-1}),ce(["MMM","MMMM"],function(e,t,n,s){var i=n._locale.monthsParse(e,s,n._strict);null!=i?t[_e]=i:g(n).invalidMonth=e});var We=/D[oD]?(\[[^\[\]]*\]|\s)+MMMM?/,He="January_February_March_April_May_June_July_August_September_October_November_December".split("_");var Re="Jan_Feb_Mar_Apr_May_Jun_Jul_Aug_Sep_Oct_Nov_Dec".split("_");function Ce(e,t){var n;if(!e.isValid())return e;if("string"==typeof t)if(/^\d+$/.test(t))t=k(t);else if(!d(t=e.localeData().monthsParse(t)))return e;return n=Math.min(e.date(),Pe(e.year(),t)),e._d["set"+(e._isUTC?"UTC":"")+"Month"](t,n),e}function Fe(e){return null!=e?(Ce(this,e),c.updateOffset(this,!0),this):xe(this,"Month")}var Le=ae;var Ue=ae;function Ne(){function e(e,t){return t.length-e.length}var t,n,s=[],i=[],r=[];for(t=0;t<12;t++)n=y([2e3,t]),s.push(this.monthsShort(n,"")),i.push(this.months(n,"")),r.push(this.months(n,"")),r.push(this.monthsShort(n,""));for(s.sort(e),i.sort(e),r.sort(e),t=0;t<12;t++)s[t]=de(s[t]),i[t]=de(i[t]);for(t=0;t<24;t++)r[t]=de(r[t]);this._monthsRegex=new RegExp("^("+r.join("|")+")","i"),this._monthsShortRegex=this._monthsRegex,this._monthsStrictRegex=new RegExp("^("+i.join("|")+")","i"),this._monthsShortStrictRegex=new RegExp("^("+s.join("|")+")","i")}function Ge(e){var t=new Date(Date.UTC.apply(null,arguments));return e<100&&0<=e&&isFinite(t.getUTCFullYear())&&t.setUTCFullYear(e),t}function Ve(e,t,n){var s=7+t-n;return-((7+Ge(e,0,s).getUTCDay()-t)%7)+s-1}function Ee(e,t,n,s,i){var r,a,o=1+7*(t-1)+(7+n-s)%7+Ve(e,s,i);return o<=0?a=De(r=e-1)+o:o>De(e)?(r=e+1,a=o-De(e)):(r=e,a=o),{year:r,dayOfYear:a}}function Ie(e,t,n){var s,i,r=Ve(e.year(),t,n),a=Math.floor((e.dayOfYear()-r-1)/7)+1;return a<1?s=a+Ae(i=e.year()-1,t,n):a>Ae(e.year(),t,n)?(s=a-Ae(e.year(),t,n),i=e.year()+1):(i=e.year(),s=a),{week:s,year:i}}function Ae(e,t,n){var s=Ve(e,t,n),i=Ve(e+1,t,n);return(De(e)-s+i)/7}I("w",["ww",2],"wo","week"),I("W",["WW",2],"Wo","isoWeek"),H("week","w"),H("isoWeek","W"),L("week",5),L("isoWeek",5),ue("w",B),ue("ww",B,z),ue("W",B),ue("WW",B,z),fe(["w","ww","W","WW"],function(e,t,n,s){t[s.substr(0,1)]=k(e)});I("d",0,"do","day"),I("dd",0,0,function(e){return this.localeData().weekdaysMin(this,e)}),I("ddd",0,0,function(e){return this.localeData().weekdaysShort(this,e)}),I("dddd",0,0,function(e){return this.localeData().weekdays(this,e)}),I("e",0,0,"weekday"),I("E",0,0,"isoWeekday"),H("day","d"),H("weekday","e"),H("isoWeekday","E"),L("day",11),L("weekday",11),L("isoWeekday",11),ue("d",B),ue("e",B),ue("E",B),ue("dd",function(e,t){return t.weekdaysMinRegex(e)}),ue("ddd",function(e,t){return t.weekdaysShortRegex(e)}),ue("dddd",function(e,t){return t.weekdaysRegex(e)}),fe(["dd","ddd","dddd"],function(e,t,n,s){var i=n._locale.weekdaysParse(e,s,n._strict);null!=i?t.d=i:g(n).invalidWeekday=e}),fe(["d","e","E"],function(e,t,n,s){t[s]=k(e)});var je="Sunday_Monday_Tuesday_Wednesday_Thursday_Friday_Saturday".split("_");var Ze="Sun_Mon_Tue_Wed_Thu_Fri_Sat".split("_");var ze="Su_Mo_Tu_We_Th_Fr_Sa".split("_");var $e=ae;var qe=ae;var Je=ae;function Be(){function e(e,t){return t.length-e.length}var t,n,s,i,r,a=[],o=[],u=[],l=[];for(t=0;t<7;t++)n=y([2e3,1]).day(t),s=this.weekdaysMin(n,""),i=this.weekdaysShort(n,""),r=this.weekdays(n,""),a.push(s),o.push(i),u.push(r),l.push(s),l.push(i),l.push(r);for(a.sort(e),o.sort(e),u.sort(e),l.sort(e),t=0;t<7;t++)o[t]=de(o[t]),u[t]=de(u[t]),l[t]=de(l[t]);this._weekdaysRegex=new RegExp("^("+l.join("|")+")","i"),this._weekdaysShortRegex=this._weekdaysRegex,this._weekdaysMinRegex=this._weekdaysRegex,this._weekdaysStrictRegex=new RegExp("^("+u.join("|")+")","i"),this._weekdaysShortStrictRegex=new RegExp("^("+o.join("|")+")","i"),this._weekdaysMinStrictRegex=new RegExp("^("+a.join("|")+")","i")}function Qe(){return this.hours()%12||12}function Xe(e,t){I(e,0,0,function(){return this.localeData().meridiem(this.hours(),this.minutes(),t)})}function Ke(e,t){return t._meridiemParse}I("H",["HH",2],0,"hour"),I("h",["hh",2],0,Qe),I("k",["kk",2],0,function(){return this.hours()||24}),I("hmm",0,0,function(){return""+Qe.apply(this)+U(this.minutes(),2)}),I("hmmss",0,0,function(){return""+Qe.apply(this)+U(this.minutes(),2)+U(this.seconds(),2)}),I("Hmm",0,0,function(){return""+this.hours()+U(this.minutes(),2)}),I("Hmmss",0,0,function(){return""+this.hours()+U(this.minutes(),2)+U(this.seconds(),2)}),Xe("a",!0),Xe("A",!1),H("hour","h"),L("hour",13),ue("a",Ke),ue("A",Ke),ue("H",B),ue("h",B),ue("k",B),ue("HH",B,z),ue("hh",B,z),ue("kk",B,z),ue("hmm",Q),ue("hmmss",X),ue("Hmm",Q),ue("Hmmss",X),ce(["H","HH"],ge),ce(["k","kk"],function(e,t,n){var s=k(e);t[ge]=24===s?0:s}),ce(["a","A"],function(e,t,n){n._isPm=n._locale.isPM(e),n._meridiem=e}),ce(["h","hh"],function(e,t,n){t[ge]=k(e),g(n).bigHour=!0}),ce("hmm",function(e,t,n){var s=e.length-2;t[ge]=k(e.substr(0,s)),t[pe]=k(e.substr(s)),g(n).bigHour=!0}),ce("hmmss",function(e,t,n){var s=e.length-4,i=e.length-2;t[ge]=k(e.substr(0,s)),t[pe]=k(e.substr(s,2)),t[ve]=k(e.substr(i)),g(n).bigHour=!0}),ce("Hmm",function(e,t,n){var s=e.length-2;t[ge]=k(e.substr(0,s)),t[pe]=k(e.substr(s))}),ce("Hmmss",function(e,t,n){var s=e.length-4,i=e.length-2;t[ge]=k(e.substr(0,s)),t[pe]=k(e.substr(s,2)),t[ve]=k(e.substr(i))});var et,tt=Te("Hours",!0),nt={calendar:{sameDay:"[Today at] LT",nextDay:"[Tomorrow at] LT",nextWeek:"dddd [at] LT",lastDay:"[Yesterday at] LT",lastWeek:"[Last] dddd [at] LT",sameElse:"L"},longDateFormat:{LTS:"h:mm:ss A",LT:"h:mm A",L:"MM/DD/YYYY",LL:"MMMM D, YYYY",LLL:"MMMM D, YYYY h:mm A",LLLL:"dddd, MMMM D, YYYY h:mm A"},invalidDate:"Invalid date",ordinal:"%d",dayOfMonthOrdinalParse:/\d{1,2}/,relativeTime:{future:"in %s",past:"%s ago",s:"a few seconds",ss:"%d seconds",m:"a minute",mm:"%d minutes",h:"an hour",hh:"%d hours",d:"a day",dd:"%d days",M:"a month",MM:"%d months",y:"a year",yy:"%d years"},months:He,monthsShort:Re,week:{dow:0,doy:6},weekdays:je,weekdaysMin:ze,weekdaysShort:Ze,meridiemParse:/[ap]\.?m?\.?/i},st={},it={};function rt(e){return e?e.toLowerCase().replace("_","-"):e}function at(e){var t=null;if(!st[e]&&"undefined"!=typeof module&&module&&module.exports)try{t=et._abbr,require("./locale/"+e),ot(t)}catch(e){}return st[e]}function ot(e,t){var n;return e&&((n=l(t)?lt(e):ut(e,t))?et=n:"undefined"!=typeof console&&console.warn&&console.warn("Locale "+e+" not found. Did you forget to load it?")),et._abbr}function ut(e,t){if(null!==t){var n,s=nt;if(t.abbr=e,null!=st[e])T("defineLocaleOverride","use moment.updateLocale(localeName, config) to change an existing locale. moment.defineLocale(localeName, config) should only be used for creating a new locale See http://momentjs.com/guides/#/warnings/define-locale/ for more info."),s=st[e]._config;else if(null!=t.parentLocale)if(null!=st[t.parentLocale])s=st[t.parentLocale]._config;else{if(null==(n=at(t.parentLocale)))return it[t.parentLocale]||(it[t.parentLocale]=[]),it[t.parentLocale].push({name:e,config:t}),null;s=n._config}return st[e]=new P(b(s,t)),it[e]&&it[e].forEach(function(e){ut(e.name,e.config)}),ot(e),st[e]}return delete st[e],null}function lt(e){var t;if(e&&e._locale&&e._locale._abbr&&(e=e._locale._abbr),!e)return et;if(!o(e)){if(t=at(e))return t;e=[e]}return function(e){for(var t,n,s,i,r=0;r<e.length;){for(t=(i=rt(e[r]).split("-")).length,n=(n=rt(e[r+1]))?n.split("-"):null;0<t;){if(s=at(i.slice(0,t).join("-")))return s;if(n&&n.length>=t&&a(i,n,!0)>=t-1)break;t--}r++}return et}(e)}function dt(e){var t,n=e._a;return n&&-2===g(e).overflow&&(t=n[_e]<0||11<n[_e]?_e:n[ye]<1||n[ye]>Pe(n[me],n[_e])?ye:n[ge]<0||24<n[ge]||24===n[ge]&&(0!==n[pe]||0!==n[ve]||0!==n[we])?ge:n[pe]<0||59<n[pe]?pe:n[ve]<0||59<n[ve]?ve:n[we]<0||999<n[we]?we:-1,g(e)._overflowDayOfYear&&(t<me||ye<t)&&(t=ye),g(e)._overflowWeeks&&-1===t&&(t=Me),g(e)._overflowWeekday&&-1===t&&(t=Se),g(e).overflow=t),e}function ht(e,t,n){return null!=e?e:null!=t?t:n}function ct(e){var t,n,s,i,r,a=[];if(!e._d){var o,u;for(o=e,u=new Date(c.now()),s=o._useUTC?[u.getUTCFullYear(),u.getUTCMonth(),u.getUTCDate()]:[u.getFullYear(),u.getMonth(),u.getDate()],e._w&&null==e._a[ye]&&null==e._a[_e]&&function(e){var t,n,s,i,r,a,o,u;if(null!=(t=e._w).GG||null!=t.W||null!=t.E)r=1,a=4,n=ht(t.GG,e._a[me],Ie(Tt(),1,4).year),s=ht(t.W,1),((i=ht(t.E,1))<1||7<i)&&(u=!0);else{r=e._locale._week.dow,a=e._locale._week.doy;var l=Ie(Tt(),r,a);n=ht(t.gg,e._a[me],l.year),s=ht(t.w,l.week),null!=t.d?((i=t.d)<0||6<i)&&(u=!0):null!=t.e?(i=t.e+r,(t.e<0||6<t.e)&&(u=!0)):i=r}s<1||s>Ae(n,r,a)?g(e)._overflowWeeks=!0:null!=u?g(e)._overflowWeekday=!0:(o=Ee(n,s,i,r,a),e._a[me]=o.year,e._dayOfYear=o.dayOfYear)}(e),null!=e._dayOfYear&&(r=ht(e._a[me],s[me]),(e._dayOfYear>De(r)||0===e._dayOfYear)&&(g(e)._overflowDayOfYear=!0),n=Ge(r,0,e._dayOfYear),e._a[_e]=n.getUTCMonth(),e._a[ye]=n.getUTCDate()),t=0;t<3&&null==e._a[t];++t)e._a[t]=a[t]=s[t];for(;t<7;t++)e._a[t]=a[t]=null==e._a[t]?2===t?1:0:e._a[t];24===e._a[ge]&&0===e._a[pe]&&0===e._a[ve]&&0===e._a[we]&&(e._nextDay=!0,e._a[ge]=0),e._d=(e._useUTC?Ge:function(e,t,n,s,i,r,a){var o=new Date(e,t,n,s,i,r,a);return e<100&&0<=e&&isFinite(o.getFullYear())&&o.setFullYear(e),o}).apply(null,a),i=e._useUTC?e._d.getUTCDay():e._d.getDay(),null!=e._tzm&&e._d.setUTCMinutes(e._d.getUTCMinutes()-e._tzm),e._nextDay&&(e._a[ge]=24),e._w&&void 0!==e._w.d&&e._w.d!==i&&(g(e).weekdayMismatch=!0)}}var ft=/^\s*((?:[+-]\d{6}|\d{4})-(?:\d\d-\d\d|W\d\d-\d|W\d\d|\d\d\d|\d\d))(?:(T| )(\d\d(?::\d\d(?::\d\d(?:[.,]\d+)?)?)?)([\+\-]\d\d(?::?\d\d)?|\s*Z)?)?$/,mt=/^\s*((?:[+-]\d{6}|\d{4})(?:\d\d\d\d|W\d\d\d|W\d\d|\d\d\d|\d\d))(?:(T| )(\d\d(?:\d\d(?:\d\d(?:[.,]\d+)?)?)?)([\+\-]\d\d(?::?\d\d)?|\s*Z)?)?$/,_t=/Z|[+-]\d\d(?::?\d\d)?/,yt=[["YYYYYY-MM-DD",/[+-]\d{6}-\d\d-\d\d/],["YYYY-MM-DD",/\d{4}-\d\d-\d\d/],["GGGG-[W]WW-E",/\d{4}-W\d\d-\d/],["GGGG-[W]WW",/\d{4}-W\d\d/,!1],["YYYY-DDD",/\d{4}-\d{3}/],["YYYY-MM",/\d{4}-\d\d/,!1],["YYYYYYMMDD",/[+-]\d{10}/],["YYYYMMDD",/\d{8}/],["GGGG[W]WWE",/\d{4}W\d{3}/],["GGGG[W]WW",/\d{4}W\d{2}/,!1],["YYYYDDD",/\d{7}/]],gt=[["HH:mm:ss.SSSS",/\d\d:\d\d:\d\d\.\d+/],["HH:mm:ss,SSSS",/\d\d:\d\d:\d\d,\d+/],["HH:mm:ss",/\d\d:\d\d:\d\d/],["HH:mm",/\d\d:\d\d/],["HHmmss.SSSS",/\d\d\d\d\d\d\.\d+/],["HHmmss,SSSS",/\d\d\d\d\d\d,\d+/],["HHmmss",/\d\d\d\d\d\d/],["HHmm",/\d\d\d\d/],["HH",/\d\d/]],pt=/^\/?Date\((\-?\d+)/i;function vt(e){var t,n,s,i,r,a,o=e._i,u=ft.exec(o)||mt.exec(o);if(u){for(g(e).iso=!0,t=0,n=yt.length;t<n;t++)if(yt[t][1].exec(u[1])){i=yt[t][0],s=!1!==yt[t][2];break}if(null==i)return void(e._isValid=!1);if(u[3]){for(t=0,n=gt.length;t<n;t++)if(gt[t][1].exec(u[3])){r=(u[2]||" ")+gt[t][0];break}if(null==r)return void(e._isValid=!1)}if(!s&&null!=r)return void(e._isValid=!1);if(u[4]){if(!_t.exec(u[4]))return void(e._isValid=!1);a="Z"}e._f=i+(r||"")+(a||""),kt(e)}else e._isValid=!1}var wt=/^(?:(Mon|Tue|Wed|Thu|Fri|Sat|Sun),?\s)?(\d{1,2})\s(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s(\d{2,4})\s(\d\d):(\d\d)(?::(\d\d))?\s(?:(UT|GMT|[ECMP][SD]T)|([Zz])|([+-]\d{4}))$/;function Mt(e,t,n,s,i,r){var a=[function(e){var t=parseInt(e,10);{if(t<=49)return 2e3+t;if(t<=999)return 1900+t}return t}(e),Re.indexOf(t),parseInt(n,10),parseInt(s,10),parseInt(i,10)];return r&&a.push(parseInt(r,10)),a}var St={UT:0,GMT:0,EDT:-240,EST:-300,CDT:-300,CST:-360,MDT:-360,MST:-420,PDT:-420,PST:-480};function Dt(e){var t,n,s,i=wt.exec(e._i.replace(/\([^)]*\)|[\n\t]/g," ").replace(/(\s\s+)/g," ").replace(/^\s\s*/,"").replace(/\s\s*$/,""));if(i){var r=Mt(i[4],i[3],i[2],i[5],i[6],i[7]);if(t=i[1],n=r,s=e,t&&Ze.indexOf(t)!==new Date(n[0],n[1],n[2]).getDay()&&(g(s).weekdayMismatch=!0,!(s._isValid=!1)))return;e._a=r,e._tzm=function(e,t,n){if(e)return St[e];if(t)return 0;var s=parseInt(n,10),i=s%100;return(s-i)/100*60+i}(i[8],i[9],i[10]),e._d=Ge.apply(null,e._a),e._d.setUTCMinutes(e._d.getUTCMinutes()-e._tzm),g(e).rfc2822=!0}else e._isValid=!1}function kt(e){if(e._f!==c.ISO_8601)if(e._f!==c.RFC_2822){e._a=[],g(e).empty=!0;var t,n,s,i,r,a,o,u,l=""+e._i,d=l.length,h=0;for(s=j(e._f,e._locale).match(N)||[],t=0;t<s.length;t++)i=s[t],(n=(l.match(le(i,e))||[])[0])&&(0<(r=l.substr(0,l.indexOf(n))).length&&g(e).unusedInput.push(r),l=l.slice(l.indexOf(n)+n.length),h+=n.length),E[i]?(n?g(e).empty=!1:g(e).unusedTokens.push(i),a=i,u=e,null!=(o=n)&&m(he,a)&&he[a](o,u._a,u,a)):e._strict&&!n&&g(e).unusedTokens.push(i);g(e).charsLeftOver=d-h,0<l.length&&g(e).unusedInput.push(l),e._a[ge]<=12&&!0===g(e).bigHour&&0<e._a[ge]&&(g(e).bigHour=void 0),g(e).parsedDateParts=e._a.slice(0),g(e).meridiem=e._meridiem,e._a[ge]=function(e,t,n){var s;if(null==n)return t;return null!=e.meridiemHour?e.meridiemHour(t,n):(null!=e.isPM&&((s=e.isPM(n))&&t<12&&(t+=12),s||12!==t||(t=0)),t)}(e._locale,e._a[ge],e._meridiem),ct(e),dt(e)}else Dt(e);else vt(e)}function Yt(e){var t,n,s,i,r=e._i,a=e._f;return e._locale=e._locale||lt(e._l),null===r||void 0===a&&""===r?v({nullInput:!0}):("string"==typeof r&&(e._i=r=e._locale.preparse(r)),S(r)?new M(dt(r)):(h(r)?e._d=r:o(a)?function(e){var t,n,s,i,r;if(0===e._f.length)return g(e).invalidFormat=!0,e._d=new Date(NaN);for(i=0;i<e._f.length;i++)r=0,t=w({},e),null!=e._useUTC&&(t._useUTC=e._useUTC),t._f=e._f[i],kt(t),p(t)&&(r+=g(t).charsLeftOver,r+=10*g(t).unusedTokens.length,g(t).score=r,(null==s||r<s)&&(s=r,n=t));_(e,n||t)}(e):a?kt(e):l(n=(t=e)._i)?t._d=new Date(c.now()):h(n)?t._d=new Date(n.valueOf()):"string"==typeof n?(s=t,null===(i=pt.exec(s._i))?(vt(s),!1===s._isValid&&(delete s._isValid,Dt(s),!1===s._isValid&&(delete s._isValid,c.createFromInputFallback(s)))):s._d=new Date(+i[1])):o(n)?(t._a=f(n.slice(0),function(e){return parseInt(e,10)}),ct(t)):u(n)?function(e){if(!e._d){var t=C(e._i);e._a=f([t.year,t.month,t.day||t.date,t.hour,t.minute,t.second,t.millisecond],function(e){return e&&parseInt(e,10)}),ct(e)}}(t):d(n)?t._d=new Date(n):c.createFromInputFallback(t),p(e)||(e._d=null),e))}function Ot(e,t,n,s,i){var r,a={};return!0!==n&&!1!==n||(s=n,n=void 0),(u(e)&&function(e){if(Object.getOwnPropertyNames)return 0===Object.getOwnPropertyNames(e).length;var t;for(t in e)if(e.hasOwnProperty(t))return!1;return!0}(e)||o(e)&&0===e.length)&&(e=void 0),a._isAMomentObject=!0,a._useUTC=a._isUTC=i,a._l=n,a._i=e,a._f=t,a._strict=s,(r=new M(dt(Yt(a))))._nextDay&&(r.add(1,"d"),r._nextDay=void 0),r}function Tt(e,t,n,s){return Ot(e,t,n,s,!1)}c.createFromInputFallback=n("value provided is not in a recognized RFC2822 or ISO format. moment construction falls back to js Date(), which is not reliable across all browsers and versions. Non RFC2822/ISO date formats are discouraged and will be removed in an upcoming major release. Please refer to http://momentjs.com/guides/#/warnings/js-date/ for more info.",function(e){e._d=new Date(e._i+(e._useUTC?" UTC":""))}),c.ISO_8601=function(){},c.RFC_2822=function(){};var xt=n("moment().min is deprecated, use moment.max instead. http://momentjs.com/guides/#/warnings/min-max/",function(){var e=Tt.apply(null,arguments);return this.isValid()&&e.isValid()?e<this?this:e:v()}),bt=n("moment().max is deprecated, use moment.min instead. http://momentjs.com/guides/#/warnings/min-max/",function(){var e=Tt.apply(null,arguments);return this.isValid()&&e.isValid()?this<e?this:e:v()});function Pt(e,t){var n,s;if(1===t.length&&o(t[0])&&(t=t[0]),!t.length)return Tt();for(n=t[0],s=1;s<t.length;++s)t[s].isValid()&&!t[s][e](n)||(n=t[s]);return n}var Wt=["year","quarter","month","week","day","hour","minute","second","millisecond"];function Ht(e){var t=C(e),n=t.year||0,s=t.quarter||0,i=t.month||0,r=t.week||0,a=t.day||0,o=t.hour||0,u=t.minute||0,l=t.second||0,d=t.millisecond||0;this._isValid=function(e){for(var t in e)if(-1===Ye.call(Wt,t)||null!=e[t]&&isNaN(e[t]))return!1;for(var n=!1,s=0;s<Wt.length;++s)if(e[Wt[s]]){if(n)return!1;parseFloat(e[Wt[s]])!==k(e[Wt[s]])&&(n=!0)}return!0}(t),this._milliseconds=+d+1e3*l+6e4*u+1e3*o*60*60,this._days=+a+7*r,this._months=+i+3*s+12*n,this._data={},this._locale=lt(),this._bubble()}function Rt(e){return e instanceof Ht}function Ct(e){return e<0?-1*Math.round(-1*e):Math.round(e)}function Ft(e,n){I(e,0,0,function(){var e=this.utcOffset(),t="+";return e<0&&(e=-e,t="-"),t+U(~~(e/60),2)+n+U(~~e%60,2)})}Ft("Z",":"),Ft("ZZ",""),ue("Z",re),ue("ZZ",re),ce(["Z","ZZ"],function(e,t,n){n._useUTC=!0,n._tzm=Ut(re,e)});var Lt=/([\+\-]|\d\d)/gi;function Ut(e,t){var n=(t||"").match(e);if(null===n)return null;var s=((n[n.length-1]||[])+"").match(Lt)||["-",0,0],i=60*s[1]+k(s[2]);return 0===i?0:"+"===s[0]?i:-i}function Nt(e,t){var n,s;return t._isUTC?(n=t.clone(),s=(S(e)||h(e)?e.valueOf():Tt(e).valueOf())-n.valueOf(),n._d.setTime(n._d.valueOf()+s),c.updateOffset(n,!1),n):Tt(e).local()}function Gt(e){return 15*-Math.round(e._d.getTimezoneOffset()/15)}function Vt(){return!!this.isValid()&&(this._isUTC&&0===this._offset)}c.updateOffset=function(){};var Et=/^(\-|\+)?(?:(\d*)[. ])?(\d+)\:(\d+)(?:\:(\d+)(\.\d*)?)?$/,It=/^(-|\+)?P(?:([-+]?[0-9,.]*)Y)?(?:([-+]?[0-9,.]*)M)?(?:([-+]?[0-9,.]*)W)?(?:([-+]?[0-9,.]*)D)?(?:T(?:([-+]?[0-9,.]*)H)?(?:([-+]?[0-9,.]*)M)?(?:([-+]?[0-9,.]*)S)?)?$/;function At(e,t){var n,s,i,r=e,a=null;return Rt(e)?r={ms:e._milliseconds,d:e._days,M:e._months}:d(e)?(r={},t?r[t]=e:r.milliseconds=e):(a=Et.exec(e))?(n="-"===a[1]?-1:1,r={y:0,d:k(a[ye])*n,h:k(a[ge])*n,m:k(a[pe])*n,s:k(a[ve])*n,ms:k(Ct(1e3*a[we]))*n}):(a=It.exec(e))?(n="-"===a[1]?-1:(a[1],1),r={y:jt(a[2],n),M:jt(a[3],n),w:jt(a[4],n),d:jt(a[5],n),h:jt(a[6],n),m:jt(a[7],n),s:jt(a[8],n)}):null==r?r={}:"object"==typeof r&&("from"in r||"to"in r)&&(i=function(e,t){var n;if(!e.isValid()||!t.isValid())return{milliseconds:0,months:0};t=Nt(t,e),e.isBefore(t)?n=Zt(e,t):((n=Zt(t,e)).milliseconds=-n.milliseconds,n.months=-n.months);return n}(Tt(r.from),Tt(r.to)),(r={}).ms=i.milliseconds,r.M=i.months),s=new Ht(r),Rt(e)&&m(e,"_locale")&&(s._locale=e._locale),s}function jt(e,t){var n=e&&parseFloat(e.replace(",","."));return(isNaN(n)?0:n)*t}function Zt(e,t){var n={milliseconds:0,months:0};return n.months=t.month()-e.month()+12*(t.year()-e.year()),e.clone().add(n.months,"M").isAfter(t)&&--n.months,n.milliseconds=+t-+e.clone().add(n.months,"M"),n}function zt(s,i){return function(e,t){var n;return null===t||isNaN(+t)||(T(i,"moment()."+i+"(period, number) is deprecated. Please use moment()."+i+"(number, period). See http://momentjs.com/guides/#/warnings/add-inverted-param/ for more info."),n=e,e=t,t=n),$t(this,At(e="string"==typeof e?+e:e,t),s),this}}function $t(e,t,n,s){var i=t._milliseconds,r=Ct(t._days),a=Ct(t._months);e.isValid()&&(s=null==s||s,a&&Ce(e,xe(e,"Month")+a*n),r&&be(e,"Date",xe(e,"Date")+r*n),i&&e._d.setTime(e._d.valueOf()+i*n),s&&c.updateOffset(e,r||a))}At.fn=Ht.prototype,At.invalid=function(){return At(NaN)};var qt=zt(1,"add"),Jt=zt(-1,"subtract");function Bt(e,t){var n=12*(t.year()-e.year())+(t.month()-e.month()),s=e.clone().add(n,"months");return-(n+(t-s<0?(t-s)/(s-e.clone().add(n-1,"months")):(t-s)/(e.clone().add(n+1,"months")-s)))||0}function Qt(e){var t;return void 0===e?this._locale._abbr:(null!=(t=lt(e))&&(this._locale=t),this)}c.defaultFormat="YYYY-MM-DDTHH:mm:ssZ",c.defaultFormatUtc="YYYY-MM-DDTHH:mm:ss[Z]";var Xt=n("moment().lang() is deprecated. Instead, use moment().localeData() to get the language configuration. Use moment().locale() to change languages.",function(e){return void 0===e?this.localeData():this.locale(e)});function Kt(){return this._locale}function en(e,t){I(0,[e,e.length],0,t)}function tn(e,t,n,s,i){var r;return null==e?Ie(this,s,i).year:((r=Ae(e,s,i))<t&&(t=r),function(e,t,n,s,i){var r=Ee(e,t,n,s,i),a=Ge(r.year,0,r.dayOfYear);return this.year(a.getUTCFullYear()),this.month(a.getUTCMonth()),this.date(a.getUTCDate()),this}.call(this,e,t,n,s,i))}I(0,["gg",2],0,function(){return this.weekYear()%100}),I(0,["GG",2],0,function(){return this.isoWeekYear()%100}),en("gggg","weekYear"),en("ggggg","weekYear"),en("GGGG","isoWeekYear"),en("GGGGG","isoWeekYear"),H("weekYear","gg"),H("isoWeekYear","GG"),L("weekYear",1),L("isoWeekYear",1),ue("G",se),ue("g",se),ue("GG",B,z),ue("gg",B,z),ue("GGGG",ee,q),ue("gggg",ee,q),ue("GGGGG",te,J),ue("ggggg",te,J),fe(["gggg","ggggg","GGGG","GGGGG"],function(e,t,n,s){t[s.substr(0,2)]=k(e)}),fe(["gg","GG"],function(e,t,n,s){t[s]=c.parseTwoDigitYear(e)}),I("Q",0,"Qo","quarter"),H("quarter","Q"),L("quarter",7),ue("Q",Z),ce("Q",function(e,t){t[_e]=3*(k(e)-1)}),I("D",["DD",2],"Do","date"),H("date","D"),L("date",9),ue("D",B),ue("DD",B,z),ue("Do",function(e,t){return e?t._dayOfMonthOrdinalParse||t._ordinalParse:t._dayOfMonthOrdinalParseLenient}),ce(["D","DD"],ye),ce("Do",function(e,t){t[ye]=k(e.match(B)[0])});var nn=Te("Date",!0);I("DDD",["DDDD",3],"DDDo","dayOfYear"),H("dayOfYear","DDD"),L("dayOfYear",4),ue("DDD",K),ue("DDDD",$),ce(["DDD","DDDD"],function(e,t,n){n._dayOfYear=k(e)}),I("m",["mm",2],0,"minute"),H("minute","m"),L("minute",14),ue("m",B),ue("mm",B,z),ce(["m","mm"],pe);var sn=Te("Minutes",!1);I("s",["ss",2],0,"second"),H("second","s"),L("second",15),ue("s",B),ue("ss",B,z),ce(["s","ss"],ve);var rn,an=Te("Seconds",!1);for(I("S",0,0,function(){return~~(this.millisecond()/100)}),I(0,["SS",2],0,function(){return~~(this.millisecond()/10)}),I(0,["SSS",3],0,"millisecond"),I(0,["SSSS",4],0,function(){return 10*this.millisecond()}),I(0,["SSSSS",5],0,function(){return 100*this.millisecond()}),I(0,["SSSSSS",6],0,function(){return 1e3*this.millisecond()}),I(0,["SSSSSSS",7],0,function(){return 1e4*this.millisecond()}),I(0,["SSSSSSSS",8],0,function(){return 1e5*this.millisecond()}),I(0,["SSSSSSSSS",9],0,function(){return 1e6*this.millisecond()}),H("millisecond","ms"),L("millisecond",16),ue("S",K,Z),ue("SS",K,z),ue("SSS",K,$),rn="SSSS";rn.length<=9;rn+="S")ue(rn,ne);function on(e,t){t[we]=k(1e3*("0."+e))}for(rn="S";rn.length<=9;rn+="S")ce(rn,on);var un=Te("Milliseconds",!1);I("z",0,0,"zoneAbbr"),I("zz",0,0,"zoneName");var ln=M.prototype;function dn(e){return e}ln.add=qt,ln.calendar=function(e,t){var n=e||Tt(),s=Nt(n,this).startOf("day"),i=c.calendarFormat(this,s)||"sameElse",r=t&&(x(t[i])?t[i].call(this,n):t[i]);return this.format(r||this.localeData().calendar(i,this,Tt(n)))},ln.clone=function(){return new M(this)},ln.diff=function(e,t,n){var s,i,r;if(!this.isValid())return NaN;if(!(s=Nt(e,this)).isValid())return NaN;switch(i=6e4*(s.utcOffset()-this.utcOffset()),t=R(t)){case"year":r=Bt(this,s)/12;break;case"month":r=Bt(this,s);break;case"quarter":r=Bt(this,s)/3;break;case"second":r=(this-s)/1e3;break;case"minute":r=(this-s)/6e4;break;case"hour":r=(this-s)/36e5;break;case"day":r=(this-s-i)/864e5;break;case"week":r=(this-s-i)/6048e5;break;default:r=this-s}return n?r:D(r)},ln.endOf=function(e){return void 0===(e=R(e))||"millisecond"===e?this:("date"===e&&(e="day"),this.startOf(e).add(1,"isoWeek"===e?"week":e).subtract(1,"ms"))},ln.format=function(e){e||(e=this.isUtc()?c.defaultFormatUtc:c.defaultFormat);var t=A(this,e);return this.localeData().postformat(t)},ln.from=function(e,t){return this.isValid()&&(S(e)&&e.isValid()||Tt(e).isValid())?At({to:this,from:e}).locale(this.locale()).humanize(!t):this.localeData().invalidDate()},ln.fromNow=function(e){return this.from(Tt(),e)},ln.to=function(e,t){return this.isValid()&&(S(e)&&e.isValid()||Tt(e).isValid())?At({from:this,to:e}).locale(this.locale()).humanize(!t):this.localeData().invalidDate()},ln.toNow=function(e){return this.to(Tt(),e)},ln.get=function(e){return x(this[e=R(e)])?this[e]():this},ln.invalidAt=function(){return g(this).overflow},ln.isAfter=function(e,t){var n=S(e)?e:Tt(e);return!(!this.isValid()||!n.isValid())&&("millisecond"===(t=R(l(t)?"millisecond":t))?this.valueOf()>n.valueOf():n.valueOf()<this.clone().startOf(t).valueOf())},ln.isBefore=function(e,t){var n=S(e)?e:Tt(e);return!(!this.isValid()||!n.isValid())&&("millisecond"===(t=R(l(t)?"millisecond":t))?this.valueOf()<n.valueOf():this.clone().endOf(t).valueOf()<n.valueOf())},ln.isBetween=function(e,t,n,s){return("("===(s=s||"()")[0]?this.isAfter(e,n):!this.isBefore(e,n))&&(")"===s[1]?this.isBefore(t,n):!this.isAfter(t,n))},ln.isSame=function(e,t){var n,s=S(e)?e:Tt(e);return!(!this.isValid()||!s.isValid())&&("millisecond"===(t=R(t||"millisecond"))?this.valueOf()===s.valueOf():(n=s.valueOf(),this.clone().startOf(t).valueOf()<=n&&n<=this.clone().endOf(t).valueOf()))},ln.isSameOrAfter=function(e,t){return this.isSame(e,t)||this.isAfter(e,t)},ln.isSameOrBefore=function(e,t){return this.isSame(e,t)||this.isBefore(e,t)},ln.isValid=function(){return p(this)},ln.lang=Xt,ln.locale=Qt,ln.localeData=Kt,ln.max=bt,ln.min=xt,ln.parsingFlags=function(){return _({},g(this))},ln.set=function(e,t){if("object"==typeof e)for(var n=function(e){var t=[];for(var n in e)t.push({unit:n,priority:F[n]});return t.sort(function(e,t){return e.priority-t.priority}),t}(e=C(e)),s=0;s<n.length;s++)this[n[s].unit](e[n[s].unit]);else if(x(this[e=R(e)]))return this[e](t);return this},ln.startOf=function(e){switch(e=R(e)){case"year":this.month(0);case"quarter":case"month":this.date(1);case"week":case"isoWeek":case"day":case"date":this.hours(0);case"hour":this.minutes(0);case"minute":this.seconds(0);case"second":this.milliseconds(0)}return"week"===e&&this.weekday(0),"isoWeek"===e&&this.isoWeekday(1),"quarter"===e&&this.month(3*Math.floor(this.month()/3)),this},ln.subtract=Jt,ln.toArray=function(){var e=this;return[e.year(),e.month(),e.date(),e.hour(),e.minute(),e.second(),e.millisecond()]},ln.toObject=function(){var e=this;return{years:e.year(),months:e.month(),date:e.date(),hours:e.hours(),minutes:e.minutes(),seconds:e.seconds(),milliseconds:e.milliseconds()}},ln.toDate=function(){return new Date(this.valueOf())},ln.toISOString=function(e){if(!this.isValid())return null;var t=!0!==e,n=t?this.clone().utc():this;return n.year()<0||9999<n.year()?A(n,t?"YYYYYY-MM-DD[T]HH:mm:ss.SSS[Z]":"YYYYYY-MM-DD[T]HH:mm:ss.SSSZ"):x(Date.prototype.toISOString)?t?this.toDate().toISOString():new Date(this.valueOf()+60*this.utcOffset()*1e3).toISOString().replace("Z",A(n,"Z")):A(n,t?"YYYY-MM-DD[T]HH:mm:ss.SSS[Z]":"YYYY-MM-DD[T]HH:mm:ss.SSSZ")},ln.inspect=function(){if(!this.isValid())return"moment.invalid(/* "+this._i+" */)";var e="moment",t="";this.isLocal()||(e=0===this.utcOffset()?"moment.utc":"moment.parseZone",t="Z");var n="["+e+'("]',s=0<=this.year()&&this.year()<=9999?"YYYY":"YYYYYY",i=t+'[")]';return this.format(n+s+"-MM-DD[T]HH:mm:ss.SSS"+i)},ln.toJSON=function(){return this.isValid()?this.toISOString():null},ln.toString=function(){return this.clone().locale("en").format("ddd MMM DD YYYY HH:mm:ss [GMT]ZZ")},ln.unix=function(){return Math.floor(this.valueOf()/1e3)},ln.valueOf=function(){return this._d.valueOf()-6e4*(this._offset||0)},ln.creationData=function(){return{input:this._i,format:this._f,locale:this._locale,isUTC:this._isUTC,strict:this._strict}},ln.year=Oe,ln.isLeapYear=function(){return ke(this.year())},ln.weekYear=function(e){return tn.call(this,e,this.week(),this.weekday(),this.localeData()._week.dow,this.localeData()._week.doy)},ln.isoWeekYear=function(e){return tn.call(this,e,this.isoWeek(),this.isoWeekday(),1,4)},ln.quarter=ln.quarters=function(e){return null==e?Math.ceil((this.month()+1)/3):this.month(3*(e-1)+this.month()%3)},ln.month=Fe,ln.daysInMonth=function(){return Pe(this.year(),this.month())},ln.week=ln.weeks=function(e){var t=this.localeData().week(this);return null==e?t:this.add(7*(e-t),"d")},ln.isoWeek=ln.isoWeeks=function(e){var t=Ie(this,1,4).week;return null==e?t:this.add(7*(e-t),"d")},ln.weeksInYear=function(){var e=this.localeData()._week;return Ae(this.year(),e.dow,e.doy)},ln.isoWeeksInYear=function(){return Ae(this.year(),1,4)},ln.date=nn,ln.day=ln.days=function(e){if(!this.isValid())return null!=e?this:NaN;var t,n,s=this._isUTC?this._d.getUTCDay():this._d.getDay();return null!=e?(t=e,n=this.localeData(),e="string"!=typeof t?t:isNaN(t)?"number"==typeof(t=n.weekdaysParse(t))?t:null:parseInt(t,10),this.add(e-s,"d")):s},ln.weekday=function(e){if(!this.isValid())return null!=e?this:NaN;var t=(this.day()+7-this.localeData()._week.dow)%7;return null==e?t:this.add(e-t,"d")},ln.isoWeekday=function(e){if(!this.isValid())return null!=e?this:NaN;if(null!=e){var t=(n=e,s=this.localeData(),"string"==typeof n?s.weekdaysParse(n)%7||7:isNaN(n)?null:n);return this.day(this.day()%7?t:t-7)}return this.day()||7;var n,s},ln.dayOfYear=function(e){var t=Math.round((this.clone().startOf("day")-this.clone().startOf("year"))/864e5)+1;return null==e?t:this.add(e-t,"d")},ln.hour=ln.hours=tt,ln.minute=ln.minutes=sn,ln.second=ln.seconds=an,ln.millisecond=ln.milliseconds=un,ln.utcOffset=function(e,t,n){var s,i=this._offset||0;if(!this.isValid())return null!=e?this:NaN;if(null!=e){if("string"==typeof e){if(null===(e=Ut(re,e)))return this}else Math.abs(e)<16&&!n&&(e*=60);return!this._isUTC&&t&&(s=Gt(this)),this._offset=e,this._isUTC=!0,null!=s&&this.add(s,"m"),i!==e&&(!t||this._changeInProgress?$t(this,At(e-i,"m"),1,!1):this._changeInProgress||(this._changeInProgress=!0,c.updateOffset(this,!0),this._changeInProgress=null)),this}return this._isUTC?i:Gt(this)},ln.utc=function(e){return this.utcOffset(0,e)},ln.local=function(e){return this._isUTC&&(this.utcOffset(0,e),this._isUTC=!1,e&&this.subtract(Gt(this),"m")),this},ln.parseZone=function(){if(null!=this._tzm)this.utcOffset(this._tzm,!1,!0);else if("string"==typeof this._i){var e=Ut(ie,this._i);null!=e?this.utcOffset(e):this.utcOffset(0,!0)}return this},ln.hasAlignedHourOffset=function(e){return!!this.isValid()&&(e=e?Tt(e).utcOffset():0,(this.utcOffset()-e)%60==0)},ln.isDST=function(){return this.utcOffset()>this.clone().month(0).utcOffset()||this.utcOffset()>this.clone().month(5).utcOffset()},ln.isLocal=function(){return!!this.isValid()&&!this._isUTC},ln.isUtcOffset=function(){return!!this.isValid()&&this._isUTC},ln.isUtc=Vt,ln.isUTC=Vt,ln.zoneAbbr=function(){return this._isUTC?"UTC":""},ln.zoneName=function(){return this._isUTC?"Coordinated Universal Time":""},ln.dates=n("dates accessor is deprecated. Use date instead.",nn),ln.months=n("months accessor is deprecated. Use month instead",Fe),ln.years=n("years accessor is deprecated. Use year instead",Oe),ln.zone=n("moment().zone is deprecated, use moment().utcOffset instead. http://momentjs.com/guides/#/warnings/zone/",function(e,t){return null!=e?("string"!=typeof e&&(e=-e),this.utcOffset(e,t),this):-this.utcOffset()}),ln.isDSTShifted=n("isDSTShifted is deprecated. See http://momentjs.com/guides/#/warnings/dst-shifted/ for more information",function(){if(!l(this._isDSTShifted))return this._isDSTShifted;var e={};if(w(e,this),(e=Yt(e))._a){var t=e._isUTC?y(e._a):Tt(e._a);this._isDSTShifted=this.isValid()&&0<a(e._a,t.toArray())}else this._isDSTShifted=!1;return this._isDSTShifted});var hn=P.prototype;function cn(e,t,n,s){var i=lt(),r=y().set(s,t);return i[n](r,e)}function fn(e,t,n){if(d(e)&&(t=e,e=void 0),e=e||"",null!=t)return cn(e,t,n,"month");var s,i=[];for(s=0;s<12;s++)i[s]=cn(e,s,n,"month");return i}function mn(e,t,n,s){"boolean"==typeof e?d(t)&&(n=t,t=void 0):(t=e,e=!1,d(n=t)&&(n=t,t=void 0)),t=t||"";var i,r=lt(),a=e?r._week.dow:0;if(null!=n)return cn(t,(n+a)%7,s,"day");var o=[];for(i=0;i<7;i++)o[i]=cn(t,(i+a)%7,s,"day");return o}hn.calendar=function(e,t,n){var s=this._calendar[e]||this._calendar.sameElse;return x(s)?s.call(t,n):s},hn.longDateFormat=function(e){var t=this._longDateFormat[e],n=this._longDateFormat[e.toUpperCase()];return t||!n?t:(this._longDateFormat[e]=n.replace(/MMMM|MM|DD|dddd/g,function(e){return e.slice(1)}),this._longDateFormat[e])},hn.invalidDate=function(){return this._invalidDate},hn.ordinal=function(e){return this._ordinal.replace("%d",e)},hn.preparse=dn,hn.postformat=dn,hn.relativeTime=function(e,t,n,s){var i=this._relativeTime[n];return x(i)?i(e,t,n,s):i.replace(/%d/i,e)},hn.pastFuture=function(e,t){var n=this._relativeTime[0<e?"future":"past"];return x(n)?n(t):n.replace(/%s/i,t)},hn.set=function(e){var t,n;for(n in e)x(t=e[n])?this[n]=t:this["_"+n]=t;this._config=e,this._dayOfMonthOrdinalParseLenient=new RegExp((this._dayOfMonthOrdinalParse.source||this._ordinalParse.source)+"|"+/\d{1,2}/.source)},hn.months=function(e,t){return e?o(this._months)?this._months[e.month()]:this._months[(this._months.isFormat||We).test(t)?"format":"standalone"][e.month()]:o(this._months)?this._months:this._months.standalone},hn.monthsShort=function(e,t){return e?o(this._monthsShort)?this._monthsShort[e.month()]:this._monthsShort[We.test(t)?"format":"standalone"][e.month()]:o(this._monthsShort)?this._monthsShort:this._monthsShort.standalone},hn.monthsParse=function(e,t,n){var s,i,r;if(this._monthsParseExact)return function(e,t,n){var s,i,r,a=e.toLocaleLowerCase();if(!this._monthsParse)for(this._monthsParse=[],this._longMonthsParse=[],this._shortMonthsParse=[],s=0;s<12;++s)r=y([2e3,s]),this._shortMonthsParse[s]=this.monthsShort(r,"").toLocaleLowerCase(),this._longMonthsParse[s]=this.months(r,"").toLocaleLowerCase();return n?"MMM"===t?-1!==(i=Ye.call(this._shortMonthsParse,a))?i:null:-1!==(i=Ye.call(this._longMonthsParse,a))?i:null:"MMM"===t?-1!==(i=Ye.call(this._shortMonthsParse,a))?i:-1!==(i=Ye.call(this._longMonthsParse,a))?i:null:-1!==(i=Ye.call(this._longMonthsParse,a))?i:-1!==(i=Ye.call(this._shortMonthsParse,a))?i:null}.call(this,e,t,n);for(this._monthsParse||(this._monthsParse=[],this._longMonthsParse=[],this._shortMonthsParse=[]),s=0;s<12;s++){if(i=y([2e3,s]),n&&!this._longMonthsParse[s]&&(this._longMonthsParse[s]=new RegExp("^"+this.months(i,"").replace(".","")+"$","i"),this._shortMonthsParse[s]=new RegExp("^"+this.monthsShort(i,"").replace(".","")+"$","i")),n||this._monthsParse[s]||(r="^"+this.months(i,"")+"|^"+this.monthsShort(i,""),this._monthsParse[s]=new RegExp(r.replace(".",""),"i")),n&&"MMMM"===t&&this._longMonthsParse[s].test(e))return s;if(n&&"MMM"===t&&this._shortMonthsParse[s].test(e))return s;if(!n&&this._monthsParse[s].test(e))return s}},hn.monthsRegex=function(e){return this._monthsParseExact?(m(this,"_monthsRegex")||Ne.call(this),e?this._monthsStrictRegex:this._monthsRegex):(m(this,"_monthsRegex")||(this._monthsRegex=Ue),this._monthsStrictRegex&&e?this._monthsStrictRegex:this._monthsRegex)},hn.monthsShortRegex=function(e){return this._monthsParseExact?(m(this,"_monthsRegex")||Ne.call(this),e?this._monthsShortStrictRegex:this._monthsShortRegex):(m(this,"_monthsShortRegex")||(this._monthsShortRegex=Le),this._monthsShortStrictRegex&&e?this._monthsShortStrictRegex:this._monthsShortRegex)},hn.week=function(e){return Ie(e,this._week.dow,this._week.doy).week},hn.firstDayOfYear=function(){return this._week.doy},hn.firstDayOfWeek=function(){return this._week.dow},hn.weekdays=function(e,t){return e?o(this._weekdays)?this._weekdays[e.day()]:this._weekdays[this._weekdays.isFormat.test(t)?"format":"standalone"][e.day()]:o(this._weekdays)?this._weekdays:this._weekdays.standalone},hn.weekdaysMin=function(e){return e?this._weekdaysMin[e.day()]:this._weekdaysMin},hn.weekdaysShort=function(e){return e?this._weekdaysShort[e.day()]:this._weekdaysShort},hn.weekdaysParse=function(e,t,n){var s,i,r;if(this._weekdaysParseExact)return function(e,t,n){var s,i,r,a=e.toLocaleLowerCase();if(!this._weekdaysParse)for(this._weekdaysParse=[],this._shortWeekdaysParse=[],this._minWeekdaysParse=[],s=0;s<7;++s)r=y([2e3,1]).day(s),this._minWeekdaysParse[s]=this.weekdaysMin(r,"").toLocaleLowerCase(),this._shortWeekdaysParse[s]=this.weekdaysShort(r,"").toLocaleLowerCase(),this._weekdaysParse[s]=this.weekdays(r,"").toLocaleLowerCase();return n?"dddd"===t?-1!==(i=Ye.call(this._weekdaysParse,a))?i:null:"ddd"===t?-1!==(i=Ye.call(this._shortWeekdaysParse,a))?i:null:-1!==(i=Ye.call(this._minWeekdaysParse,a))?i:null:"dddd"===t?-1!==(i=Ye.call(this._weekdaysParse,a))?i:-1!==(i=Ye.call(this._shortWeekdaysParse,a))?i:-1!==(i=Ye.call(this._minWeekdaysParse,a))?i:null:"ddd"===t?-1!==(i=Ye.call(this._shortWeekdaysParse,a))?i:-1!==(i=Ye.call(this._weekdaysParse,a))?i:-1!==(i=Ye.call(this._minWeekdaysParse,a))?i:null:-1!==(i=Ye.call(this._minWeekdaysParse,a))?i:-1!==(i=Ye.call(this._weekdaysParse,a))?i:-1!==(i=Ye.call(this._shortWeekdaysParse,a))?i:null}.call(this,e,t,n);for(this._weekdaysParse||(this._weekdaysParse=[],this._minWeekdaysParse=[],this._shortWeekdaysParse=[],this._fullWeekdaysParse=[]),s=0;s<7;s++){if(i=y([2e3,1]).day(s),n&&!this._fullWeekdaysParse[s]&&(this._fullWeekdaysParse[s]=new RegExp("^"+this.weekdays(i,"").replace(".","\\.?")+"$","i"),this._shortWeekdaysParse[s]=new RegExp("^"+this.weekdaysShort(i,"").replace(".","\\.?")+"$","i"),this._minWeekdaysParse[s]=new RegExp("^"+this.weekdaysMin(i,"").replace(".","\\.?")+"$","i")),this._weekdaysParse[s]||(r="^"+this.weekdays(i,"")+"|^"+this.weekdaysShort(i,"")+"|^"+this.weekdaysMin(i,""),this._weekdaysParse[s]=new RegExp(r.replace(".",""),"i")),n&&"dddd"===t&&this._fullWeekdaysParse[s].test(e))return s;if(n&&"ddd"===t&&this._shortWeekdaysParse[s].test(e))return s;if(n&&"dd"===t&&this._minWeekdaysParse[s].test(e))return s;if(!n&&this._weekdaysParse[s].test(e))return s}},hn.weekdaysRegex=function(e){return this._weekdaysParseExact?(m(this,"_weekdaysRegex")||Be.call(this),e?this._weekdaysStrictRegex:this._weekdaysRegex):(m(this,"_weekdaysRegex")||(this._weekdaysRegex=$e),this._weekdaysStrictRegex&&e?this._weekdaysStrictRegex:this._weekdaysRegex)},hn.weekdaysShortRegex=function(e){return this._weekdaysParseExact?(m(this,"_weekdaysRegex")||Be.call(this),e?this._weekdaysShortStrictRegex:this._weekdaysShortRegex):(m(this,"_weekdaysShortRegex")||(this._weekdaysShortRegex=qe),this._weekdaysShortStrictRegex&&e?this._weekdaysShortStrictRegex:this._weekdaysShortRegex)},hn.weekdaysMinRegex=function(e){return this._weekdaysParseExact?(m(this,"_weekdaysRegex")||Be.call(this),e?this._weekdaysMinStrictRegex:this._weekdaysMinRegex):(m(this,"_weekdaysMinRegex")||(this._weekdaysMinRegex=Je),this._weekdaysMinStrictRegex&&e?this._weekdaysMinStrictRegex:this._weekdaysMinRegex)},hn.isPM=function(e){return"p"===(e+"").toLowerCase().charAt(0)},hn.meridiem=function(e,t,n){return 11<e?n?"pm":"PM":n?"am":"AM"},ot("en",{dayOfMonthOrdinalParse:/\d{1,2}(th|st|nd|rd)/,ordinal:function(e){var t=e%10;return e+(1===k(e%100/10)?"th":1===t?"st":2===t?"nd":3===t?"rd":"th")}}),c.lang=n("moment.lang is deprecated. Use moment.locale instead.",ot),c.langData=n("moment.langData is deprecated. Use moment.localeData instead.",lt);var _n=Math.abs;function yn(e,t,n,s){var i=At(t,n);return e._milliseconds+=s*i._milliseconds,e._days+=s*i._days,e._months+=s*i._months,e._bubble()}function gn(e){return e<0?Math.floor(e):Math.ceil(e)}function pn(e){return 4800*e/146097}function vn(e){return 146097*e/4800}function wn(e){return function(){return this.as(e)}}var Mn=wn("ms"),Sn=wn("s"),Dn=wn("m"),kn=wn("h"),Yn=wn("d"),On=wn("w"),Tn=wn("M"),xn=wn("y");function bn(e){return function(){return this.isValid()?this._data[e]:NaN}}var Pn=bn("milliseconds"),Wn=bn("seconds"),Hn=bn("minutes"),Rn=bn("hours"),Cn=bn("days"),Fn=bn("months"),Ln=bn("years");var Un=Math.round,Nn={ss:44,s:45,m:45,h:22,d:26,M:11};var Gn=Math.abs;function Vn(e){return(0<e)-(e<0)||+e}function En(){if(!this.isValid())return this.localeData().invalidDate();var e,t,n=Gn(this._milliseconds)/1e3,s=Gn(this._days),i=Gn(this._months);t=D((e=D(n/60))/60),n%=60,e%=60;var r=D(i/12),a=i%=12,o=s,u=t,l=e,d=n?n.toFixed(3).replace(/\.?0+$/,""):"",h=this.asSeconds();if(!h)return"P0D";var c=h<0?"-":"",f=Vn(this._months)!==Vn(h)?"-":"",m=Vn(this._days)!==Vn(h)?"-":"",_=Vn(this._milliseconds)!==Vn(h)?"-":"";return c+"P"+(r?f+r+"Y":"")+(a?f+a+"M":"")+(o?m+o+"D":"")+(u||l||d?"T":"")+(u?_+u+"H":"")+(l?_+l+"M":"")+(d?_+d+"S":"")}var In=Ht.prototype;return In.isValid=function(){return this._isValid},In.abs=function(){var e=this._data;return this._milliseconds=_n(this._milliseconds),this._days=_n(this._days),this._months=_n(this._months),e.milliseconds=_n(e.milliseconds),e.seconds=_n(e.seconds),e.minutes=_n(e.minutes),e.hours=_n(e.hours),e.months=_n(e.months),e.years=_n(e.years),this},In.add=function(e,t){return yn(this,e,t,1)},In.subtract=function(e,t){return yn(this,e,t,-1)},In.as=function(e){if(!this.isValid())return NaN;var t,n,s=this._milliseconds;if("month"===(e=R(e))||"year"===e)return t=this._days+s/864e5,n=this._months+pn(t),"month"===e?n:n/12;switch(t=this._days+Math.round(vn(this._months)),e){case"week":return t/7+s/6048e5;case"day":return t+s/864e5;case"hour":return 24*t+s/36e5;case"minute":return 1440*t+s/6e4;case"second":return 86400*t+s/1e3;case"millisecond":return Math.floor(864e5*t)+s;default:throw new Error("Unknown unit "+e)}},In.asMilliseconds=Mn,In.asSeconds=Sn,In.asMinutes=Dn,In.asHours=kn,In.asDays=Yn,In.asWeeks=On,In.asMonths=Tn,In.asYears=xn,In.valueOf=function(){return this.isValid()?this._milliseconds+864e5*this._days+this._months%12*2592e6+31536e6*k(this._months/12):NaN},In._bubble=function(){var e,t,n,s,i,r=this._milliseconds,a=this._days,o=this._months,u=this._data;return 0<=r&&0<=a&&0<=o||r<=0&&a<=0&&o<=0||(r+=864e5*gn(vn(o)+a),o=a=0),u.milliseconds=r%1e3,e=D(r/1e3),u.seconds=e%60,t=D(e/60),u.minutes=t%60,n=D(t/60),u.hours=n%24,o+=i=D(pn(a+=D(n/24))),a-=gn(vn(i)),s=D(o/12),o%=12,u.days=a,u.months=o,u.years=s,this},In.clone=function(){return At(this)},In.get=function(e){return e=R(e),this.isValid()?this[e+"s"]():NaN},In.milliseconds=Pn,In.seconds=Wn,In.minutes=Hn,In.hours=Rn,In.days=Cn,In.weeks=function(){return D(this.days()/7)},In.months=Fn,In.years=Ln,In.humanize=function(e){if(!this.isValid())return this.localeData().invalidDate();var t,n,s,i,r,a,o,u,l,d,h,c=this.localeData(),f=(n=!e,s=c,i=At(t=this).abs(),r=Un(i.as("s")),a=Un(i.as("m")),o=Un(i.as("h")),u=Un(i.as("d")),l=Un(i.as("M")),d=Un(i.as("y")),(h=r<=Nn.ss&&["s",r]||r<Nn.s&&["ss",r]||a<=1&&["m"]||a<Nn.m&&["mm",a]||o<=1&&["h"]||o<Nn.h&&["hh",o]||u<=1&&["d"]||u<Nn.d&&["dd",u]||l<=1&&["M"]||l<Nn.M&&["MM",l]||d<=1&&["y"]||["yy",d])[2]=n,h[3]=0<+t,h[4]=s,function(e,t,n,s,i){return i.relativeTime(t||1,!!n,e,s)}.apply(null,h));return e&&(f=c.pastFuture(+this,f)),c.postformat(f)},In.toISOString=En,In.toString=En,In.toJSON=En,In.locale=Qt,In.localeData=Kt,In.toIsoString=n("toIsoString() is deprecated. Please use toISOString() instead (notice the capitals)",En),In.lang=Xt,I("X",0,0,"unix"),I("x",0,0,"valueOf"),ue("x",se),ue("X",/[+-]?\d+(\.\d{1,3})?/),ce("X",function(e,t,n){n._d=new Date(1e3*parseFloat(e,10))}),ce("x",function(e,t,n){n._d=new Date(k(e))}),c.version="2.22.2",e=Tt,c.fn=ln,c.min=function(){return Pt("isBefore",[].slice.call(arguments,0))},c.max=function(){return Pt("isAfter",[].slice.call(arguments,0))},c.now=function(){return Date.now?Date.now():+new Date},c.utc=y,c.unix=function(e){return Tt(1e3*e)},c.months=function(e,t){return fn(e,t,"months")},c.isDate=h,c.locale=ot,c.invalid=v,c.duration=At,c.isMoment=S,c.weekdays=function(e,t,n){return mn(e,t,n,"weekdays")},c.parseZone=function(){return Tt.apply(null,arguments).parseZone()},c.localeData=lt,c.isDuration=Rt,c.monthsShort=function(e,t){return fn(e,t,"monthsShort")},c.weekdaysMin=function(e,t,n){return mn(e,t,n,"weekdaysMin")},c.defineLocale=ut,c.updateLocale=function(e,t){if(null!=t){var n,s,i=nt;null!=(s=at(e))&&(i=s._config),(n=new P(t=b(i,t))).parentLocale=st[e],st[e]=n,ot(e)}else null!=st[e]&&(null!=st[e].parentLocale?st[e]=st[e].parentLocale:null!=st[e]&&delete st[e]);return st[e]},c.locales=function(){return s(st)},c.weekdaysShort=function(e,t,n){return mn(e,t,n,"weekdaysShort")},c.normalizeUnits=R,c.relativeTimeRounding=function(e){return void 0===e?Un:"function"==typeof e&&(Un=e,!0)},c.relativeTimeThreshold=function(e,t){return void 0!==Nn[e]&&(void 0===t?Nn[e]:(Nn[e]=t,"s"===e&&(Nn.ss=t-1),!0))},c.calendarFormat=function(e,t){var n=e.diff(t,"days",!0);return n<-6?"sameElse":n<-1?"lastWeek":n<0?"lastDay":n<1?"sameDay":n<2?"nextDay":n<7?"nextWeek":"sameElse"},c.prototype=ln,c.HTML5_FMT={DATETIME_LOCAL:"YYYY-MM-DDTHH:mm",DATETIME_LOCAL_SECONDS:"YYYY-MM-DDTHH:mm:ss",DATETIME_LOCAL_MS:"YYYY-MM-DDTHH:mm:ss.SSS",DATE:"YYYY-MM-DD",TIME:"HH:mm",TIME_SECONDS:"HH:mm:ss",TIME_MS:"HH:mm:ss.SSS",WEEK:"YYYY-[W]WW",MONTH:"YYYY-MM"},c});

(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module unless amdModuleId is set
    define('Chartist', [], function () {
      return (root['Chartist'] = factory());
    });
  } else if (typeof module === 'object' && module.exports) {
    // Node. Does not work with strict CommonJS, but
    // only CommonJS-like environments that support module.exports,
    // like Node.
    module.exports = factory();
  } else {
    root['Chartist'] = factory();
  }
}(this, function () {

/* Chartist.js 0.11.0
 * Copyright © 2017 Gion Kunz
 * Free to use under either the WTFPL license or the MIT license.
 * https://raw.githubusercontent.com/gionkunz/chartist-js/master/LICENSE-WTFPL
 * https://raw.githubusercontent.com/gionkunz/chartist-js/master/LICENSE-MIT
 */
/**
 * The core module of Chartist that is mainly providing static functions and higher level functions for chart modules.
 *
 * @module Chartist.Core
 */
var Chartist = {
  version: '0.11.0'
};

(function (window, document, Chartist) {
  'use strict';

  /**
   * This object contains all namespaces used within Chartist.
   *
   * @memberof Chartist.Core
   * @type {{svg: string, xmlns: string, xhtml: string, xlink: string, ct: string}}
   */
  Chartist.namespaces = {
    svg: 'http://www.w3.org/2000/svg',
    xmlns: 'http://www.w3.org/2000/xmlns/',
    xhtml: 'http://www.w3.org/1999/xhtml',
    xlink: 'http://www.w3.org/1999/xlink',
    ct: 'http://gionkunz.github.com/chartist-js/ct'
  };

  /**
   * Helps to simplify functional style code
   *
   * @memberof Chartist.Core
   * @param {*} n This exact value will be returned by the noop function
   * @return {*} The same value that was provided to the n parameter
   */
  Chartist.noop = function (n) {
    return n;
  };

  /**
   * Generates a-z from a number 0 to 26
   *
   * @memberof Chartist.Core
   * @param {Number} n A number from 0 to 26 that will result in a letter a-z
   * @return {String} A character from a-z based on the input number n
   */
  Chartist.alphaNumerate = function (n) {
    // Limit to a-z
    return String.fromCharCode(97 + n % 26);
  };

  /**
   * Simple recursive object extend
   *
   * @memberof Chartist.Core
   * @param {Object} target Target object where the source will be merged into
   * @param {Object...} sources This object (objects) will be merged into target and then target is returned
   * @return {Object} An object that has the same reference as target but is extended and merged with the properties of source
   */
  Chartist.extend = function (target) {
    var i, source, sourceProp;
    target = target || {};

    for (i = 1; i < arguments.length; i++) {
      source = arguments[i];
      for (var prop in source) {
        sourceProp = source[prop];
        if (typeof sourceProp === 'object' && sourceProp !== null && !(sourceProp instanceof Array)) {
          target[prop] = Chartist.extend(target[prop], sourceProp);
        } else {
          target[prop] = sourceProp;
        }
      }
    }

    return target;
  };

  /**
   * Replaces all occurrences of subStr in str with newSubStr and returns a new string.
   *
   * @memberof Chartist.Core
   * @param {String} str
   * @param {String} subStr
   * @param {String} newSubStr
   * @return {String}
   */
  Chartist.replaceAll = function(str, subStr, newSubStr) {
    return str.replace(new RegExp(subStr, 'g'), newSubStr);
  };

  /**
   * Converts a number to a string with a unit. If a string is passed then this will be returned unmodified.
   *
   * @memberof Chartist.Core
   * @param {Number} value
   * @param {String} unit
   * @return {String} Returns the passed number value with unit.
   */
  Chartist.ensureUnit = function(value, unit) {
    if(typeof value === 'number') {
      value = value + unit;
    }

    return value;
  };

  /**
   * Converts a number or string to a quantity object.
   *
   * @memberof Chartist.Core
   * @param {String|Number} input
   * @return {Object} Returns an object containing the value as number and the unit as string.
   */
  Chartist.quantity = function(input) {
    if (typeof input === 'string') {
      var match = (/^(\d+)\s*(.*)$/g).exec(input);
      return {
        value : +match[1],
        unit: match[2] || undefined
      };
    }
    return { value: input };
  };

  /**
   * This is a wrapper around document.querySelector that will return the query if it's already of type Node
   *
   * @memberof Chartist.Core
   * @param {String|Node} query The query to use for selecting a Node or a DOM node that will be returned directly
   * @return {Node}
   */
  Chartist.querySelector = function(query) {
    return query instanceof Node ? query : document.querySelector(query);
  };

  /**
   * Functional style helper to produce array with given length initialized with undefined values
   *
   * @memberof Chartist.Core
   * @param length
   * @return {Array}
   */
  Chartist.times = function(length) {
    return Array.apply(null, new Array(length));
  };

  /**
   * Sum helper to be used in reduce functions
   *
   * @memberof Chartist.Core
   * @param previous
   * @param current
   * @return {*}
   */
  Chartist.sum = function(previous, current) {
    return previous + (current ? current : 0);
  };

  /**
   * Multiply helper to be used in `Array.map` for multiplying each value of an array with a factor.
   *
   * @memberof Chartist.Core
   * @param {Number} factor
   * @returns {Function} Function that can be used in `Array.map` to multiply each value in an array
   */
  Chartist.mapMultiply = function(factor) {
    return function(num) {
      return num * factor;
    };
  };

  /**
   * Add helper to be used in `Array.map` for adding a addend to each value of an array.
   *
   * @memberof Chartist.Core
   * @param {Number} addend
   * @returns {Function} Function that can be used in `Array.map` to add a addend to each value in an array
   */
  Chartist.mapAdd = function(addend) {
    return function(num) {
      return num + addend;
    };
  };

  /**
   * Map for multi dimensional arrays where their nested arrays will be mapped in serial. The output array will have the length of the largest nested array. The callback function is called with variable arguments where each argument is the nested array value (or undefined if there are no more values).
   *
   * @memberof Chartist.Core
   * @param arr
   * @param cb
   * @return {Array}
   */
  Chartist.serialMap = function(arr, cb) {
    var result = [],
        length = Math.max.apply(null, arr.map(function(e) {
          return e.length;
        }));

    Chartist.times(length).forEach(function(e, index) {
      var args = arr.map(function(e) {
        return e[index];
      });

      result[index] = cb.apply(null, args);
    });

    return result;
  };

  /**
   * This helper function can be used to round values with certain precision level after decimal. This is used to prevent rounding errors near float point precision limit.
   *
   * @memberof Chartist.Core
   * @param {Number} value The value that should be rounded with precision
   * @param {Number} [digits] The number of digits after decimal used to do the rounding
   * @returns {number} Rounded value
   */
  Chartist.roundWithPrecision = function(value, digits) {
    var precision = Math.pow(10, digits || Chartist.precision);
    return Math.round(value * precision) / precision;
  };

  /**
   * Precision level used internally in Chartist for rounding. If you require more decimal places you can increase this number.
   *
   * @memberof Chartist.Core
   * @type {number}
   */
  Chartist.precision = 8;

  /**
   * A map with characters to escape for strings to be safely used as attribute values.
   *
   * @memberof Chartist.Core
   * @type {Object}
   */
  Chartist.escapingMap = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    '\'': '&#039;'
  };

  /**
   * This function serializes arbitrary data to a string. In case of data that can't be easily converted to a string, this function will create a wrapper object and serialize the data using JSON.stringify. The outcoming string will always be escaped using Chartist.escapingMap.
   * If called with null or undefined the function will return immediately with null or undefined.
   *
   * @memberof Chartist.Core
   * @param {Number|String|Object} data
   * @return {String}
   */
  Chartist.serialize = function(data) {
    if(data === null || data === undefined) {
      return data;
    } else if(typeof data === 'number') {
      data = ''+data;
    } else if(typeof data === 'object') {
      data = JSON.stringify({data: data});
    }

    return Object.keys(Chartist.escapingMap).reduce(function(result, key) {
      return Chartist.replaceAll(result, key, Chartist.escapingMap[key]);
    }, data);
  };

  /**
   * This function de-serializes a string previously serialized with Chartist.serialize. The string will always be unescaped using Chartist.escapingMap before it's returned. Based on the input value the return type can be Number, String or Object. JSON.parse is used with try / catch to see if the unescaped string can be parsed into an Object and this Object will be returned on success.
   *
   * @memberof Chartist.Core
   * @param {String} data
   * @return {String|Number|Object}
   */
  Chartist.deserialize = function(data) {
    if(typeof data !== 'string') {
      return data;
    }

    data = Object.keys(Chartist.escapingMap).reduce(function(result, key) {
      return Chartist.replaceAll(result, Chartist.escapingMap[key], key);
    }, data);

    try {
      data = JSON.parse(data);
      data = data.data !== undefined ? data.data : data;
    } catch(e) {}

    return data;
  };

  /**
   * Create or reinitialize the SVG element for the chart
   *
   * @memberof Chartist.Core
   * @param {Node} container The containing DOM Node object that will be used to plant the SVG element
   * @param {String} width Set the width of the SVG element. Default is 100%
   * @param {String} height Set the height of the SVG element. Default is 100%
   * @param {String} className Specify a class to be added to the SVG element
   * @return {Object} The created/reinitialized SVG element
   */
  Chartist.createSvg = function (container, width, height, className) {
    var svg;

    width = width || '100%';
    height = height || '100%';

    // Check if there is a previous SVG element in the container that contains the Chartist XML namespace and remove it
    // Since the DOM API does not support namespaces we need to manually search the returned list http://www.w3.org/TR/selectors-api/
    Array.prototype.slice.call(container.querySelectorAll('svg')).filter(function filterChartistSvgObjects(svg) {
      return svg.getAttributeNS(Chartist.namespaces.xmlns, 'ct');
    }).forEach(function removePreviousElement(svg) {
      container.removeChild(svg);
    });

    // Create svg object with width and height or use 100% as default
    svg = new Chartist.Svg('svg').attr({
      width: width,
      height: height
    }).addClass(className);

    svg._node.style.width = width;
    svg._node.style.height = height;

    // Add the DOM node to our container
    container.appendChild(svg._node);

    return svg;
  };

  /**
   * Ensures that the data object passed as second argument to the charts is present and correctly initialized.
   *
   * @param  {Object} data The data object that is passed as second argument to the charts
   * @return {Object} The normalized data object
   */
  Chartist.normalizeData = function(data, reverse, multi) {
    var labelCount;
    var output = {
      raw: data,
      normalized: {}
    };

    // Check if we should generate some labels based on existing series data
    output.normalized.series = Chartist.getDataArray({
      series: data.series || []
    }, reverse, multi);

    // If all elements of the normalized data array are arrays we're dealing with
    // multi series data and we need to find the largest series if they are un-even
    if (output.normalized.series.every(function(value) {
        return value instanceof Array;
      })) {
      // Getting the series with the the most elements
      labelCount = Math.max.apply(null, output.normalized.series.map(function(series) {
        return series.length;
      }));
    } else {
      // We're dealing with Pie data so we just take the normalized array length
      labelCount = output.normalized.series.length;
    }

    output.normalized.labels = (data.labels || []).slice();
    // Padding the labels to labelCount with empty strings
    Array.prototype.push.apply(
      output.normalized.labels,
      Chartist.times(Math.max(0, labelCount - output.normalized.labels.length)).map(function() {
        return '';
      })
    );

    if(reverse) {
      Chartist.reverseData(output.normalized);
    }

    return output;
  };

  /**
   * This function safely checks if an objects has an owned property.
   *
   * @param {Object} object The object where to check for a property
   * @param {string} property The property name
   * @returns {boolean} Returns true if the object owns the specified property
   */
  Chartist.safeHasProperty = function(object, property) {
    return object !== null &&
      typeof object === 'object' &&
      object.hasOwnProperty(property);
  };

  /**
   * Checks if a value is considered a hole in the data series.
   *
   * @param {*} value
   * @returns {boolean} True if the value is considered a data hole
   */
  Chartist.isDataHoleValue = function(value) {
    return value === null ||
      value === undefined ||
      (typeof value === 'number' && isNaN(value));
  };

  /**
   * Reverses the series, labels and series data arrays.
   *
   * @memberof Chartist.Core
   * @param data
   */
  Chartist.reverseData = function(data) {
    data.labels.reverse();
    data.series.reverse();
    for (var i = 0; i < data.series.length; i++) {
      if(typeof(data.series[i]) === 'object' && data.series[i].data !== undefined) {
        data.series[i].data.reverse();
      } else if(data.series[i] instanceof Array) {
        data.series[i].reverse();
      }
    }
  };

  /**
   * Convert data series into plain array
   *
   * @memberof Chartist.Core
   * @param {Object} data The series object that contains the data to be visualized in the chart
   * @param {Boolean} [reverse] If true the whole data is reversed by the getDataArray call. This will modify the data object passed as first parameter. The labels as well as the series order is reversed. The whole series data arrays are reversed too.
   * @param {Boolean} [multi] Create a multi dimensional array from a series data array where a value object with `x` and `y` values will be created.
   * @return {Array} A plain array that contains the data to be visualized in the chart
   */
  Chartist.getDataArray = function(data, reverse, multi) {
    // Recursively walks through nested arrays and convert string values to numbers and objects with value properties
    // to values. Check the tests in data core -> data normalization for a detailed specification of expected values
    function recursiveConvert(value) {
      if(Chartist.safeHasProperty(value, 'value')) {
        // We are dealing with value object notation so we need to recurse on value property
        return recursiveConvert(value.value);
      } else if(Chartist.safeHasProperty(value, 'data')) {
        // We are dealing with series object notation so we need to recurse on data property
        return recursiveConvert(value.data);
      } else if(value instanceof Array) {
        // Data is of type array so we need to recurse on the series
        return value.map(recursiveConvert);
      } else if(Chartist.isDataHoleValue(value)) {
        // We're dealing with a hole in the data and therefore need to return undefined
        // We're also returning undefined for multi value output
        return undefined;
      } else {
        // We need to prepare multi value output (x and y data)
        if(multi) {
          var multiValue = {};

          // Single series value arrays are assumed to specify the Y-Axis value
          // For example: [1, 2] => [{x: undefined, y: 1}, {x: undefined, y: 2}]
          // If multi is a string then it's assumed that it specified which dimension should be filled as default
          if(typeof multi === 'string') {
            multiValue[multi] = Chartist.getNumberOrUndefined(value);
          } else {
            multiValue.y = Chartist.getNumberOrUndefined(value);
          }

          multiValue.x = value.hasOwnProperty('x') ? Chartist.getNumberOrUndefined(value.x) : multiValue.x;
          multiValue.y = value.hasOwnProperty('y') ? Chartist.getNumberOrUndefined(value.y) : multiValue.y;

          return multiValue;

        } else {
          // We can return simple data
          return Chartist.getNumberOrUndefined(value);
        }
      }
    }

    return data.series.map(recursiveConvert);
  };

  /**
   * Converts a number into a padding object.
   *
   * @memberof Chartist.Core
   * @param {Object|Number} padding
   * @param {Number} [fallback] This value is used to fill missing values if a incomplete padding object was passed
   * @returns {Object} Returns a padding object containing top, right, bottom, left properties filled with the padding number passed in as argument. If the argument is something else than a number (presumably already a correct padding object) then this argument is directly returned.
   */
  Chartist.normalizePadding = function(padding, fallback) {
    fallback = fallback || 0;

    return typeof padding === 'number' ? {
      top: padding,
      right: padding,
      bottom: padding,
      left: padding
    } : {
      top: typeof padding.top === 'number' ? padding.top : fallback,
      right: typeof padding.right === 'number' ? padding.right : fallback,
      bottom: typeof padding.bottom === 'number' ? padding.bottom : fallback,
      left: typeof padding.left === 'number' ? padding.left : fallback
    };
  };

  Chartist.getMetaData = function(series, index) {
    var value = series.data ? series.data[index] : series[index];
    return value ? value.meta : undefined;
  };

  /**
   * Calculate the order of magnitude for the chart scale
   *
   * @memberof Chartist.Core
   * @param {Number} value The value Range of the chart
   * @return {Number} The order of magnitude
   */
  Chartist.orderOfMagnitude = function (value) {
    return Math.floor(Math.log(Math.abs(value)) / Math.LN10);
  };

  /**
   * Project a data length into screen coordinates (pixels)
   *
   * @memberof Chartist.Core
   * @param {Object} axisLength The svg element for the chart
   * @param {Number} length Single data value from a series array
   * @param {Object} bounds All the values to set the bounds of the chart
   * @return {Number} The projected data length in pixels
   */
  Chartist.projectLength = function (axisLength, length, bounds) {
    return length / bounds.range * axisLength;
  };

  /**
   * Get the height of the area in the chart for the data series
   *
   * @memberof Chartist.Core
   * @param {Object} svg The svg element for the chart
   * @param {Object} options The Object that contains all the optional values for the chart
   * @return {Number} The height of the area in the chart for the data series
   */
  Chartist.getAvailableHeight = function (svg, options) {
    return Math.max((Chartist.quantity(options.height).value || svg.height()) - (options.chartPadding.top +  options.chartPadding.bottom) - options.axisX.offset, 0);
  };

  /**
   * Get highest and lowest value of data array. This Array contains the data that will be visualized in the chart.
   *
   * @memberof Chartist.Core
   * @param {Array} data The array that contains the data to be visualized in the chart
   * @param {Object} options The Object that contains the chart options
   * @param {String} dimension Axis dimension 'x' or 'y' used to access the correct value and high / low configuration
   * @return {Object} An object that contains the highest and lowest value that will be visualized on the chart.
   */
  Chartist.getHighLow = function (data, options, dimension) {
    // TODO: Remove workaround for deprecated global high / low config. Axis high / low configuration is preferred
    options = Chartist.extend({}, options, dimension ? options['axis' + dimension.toUpperCase()] : {});

    var highLow = {
        high: options.high === undefined ? -Number.MAX_VALUE : +options.high,
        low: options.low === undefined ? Number.MAX_VALUE : +options.low
      };
    var findHigh = options.high === undefined;
    var findLow = options.low === undefined;

    // Function to recursively walk through arrays and find highest and lowest number
    function recursiveHighLow(data) {
      if(data === undefined) {
        return undefined;
      } else if(data instanceof Array) {
        for (var i = 0; i < data.length; i++) {
          recursiveHighLow(data[i]);
        }
      } else {
        var value = dimension ? +data[dimension] : +data;

        if (findHigh && value > highLow.high) {
          highLow.high = value;
        }

        if (findLow && value < highLow.low) {
          highLow.low = value;
        }
      }
    }

    // Start to find highest and lowest number recursively
    if(findHigh || findLow) {
      recursiveHighLow(data);
    }

    // Overrides of high / low based on reference value, it will make sure that the invisible reference value is
    // used to generate the chart. This is useful when the chart always needs to contain the position of the
    // invisible reference value in the view i.e. for bipolar scales.
    if (options.referenceValue || options.referenceValue === 0) {
      highLow.high = Math.max(options.referenceValue, highLow.high);
      highLow.low = Math.min(options.referenceValue, highLow.low);
    }

    // If high and low are the same because of misconfiguration or flat data (only the same value) we need
    // to set the high or low to 0 depending on the polarity
    if (highLow.high <= highLow.low) {
      // If both values are 0 we set high to 1
      if (highLow.low === 0) {
        highLow.high = 1;
      } else if (highLow.low < 0) {
        // If we have the same negative value for the bounds we set bounds.high to 0
        highLow.high = 0;
      } else if (highLow.high > 0) {
        // If we have the same positive value for the bounds we set bounds.low to 0
        highLow.low = 0;
      } else {
        // If data array was empty, values are Number.MAX_VALUE and -Number.MAX_VALUE. Set bounds to prevent errors
        highLow.high = 1;
        highLow.low = 0;
      }
    }

    return highLow;
  };

  /**
   * Checks if a value can be safely coerced to a number. This includes all values except null which result in finite numbers when coerced. This excludes NaN, since it's not finite.
   *
   * @memberof Chartist.Core
   * @param value
   * @returns {Boolean}
   */
  Chartist.isNumeric = function(value) {
    return value === null ? false : isFinite(value);
  };

  /**
   * Returns true on all falsey values except the numeric value 0.
   *
   * @memberof Chartist.Core
   * @param value
   * @returns {boolean}
   */
  Chartist.isFalseyButZero = function(value) {
    return !value && value !== 0;
  };

  /**
   * Returns a number if the passed parameter is a valid number or the function will return undefined. On all other values than a valid number, this function will return undefined.
   *
   * @memberof Chartist.Core
   * @param value
   * @returns {*}
   */
  Chartist.getNumberOrUndefined = function(value) {
    return Chartist.isNumeric(value) ? +value : undefined;
  };

  /**
   * Checks if provided value object is multi value (contains x or y properties)
   *
   * @memberof Chartist.Core
   * @param value
   */
  Chartist.isMultiValue = function(value) {
    return typeof value === 'object' && ('x' in value || 'y' in value);
  };

  /**
   * Gets a value from a dimension `value.x` or `value.y` while returning value directly if it's a valid numeric value. If the value is not numeric and it's falsey this function will return `defaultValue`.
   *
   * @memberof Chartist.Core
   * @param value
   * @param dimension
   * @param defaultValue
   * @returns {*}
   */
  Chartist.getMultiValue = function(value, dimension) {
    if(Chartist.isMultiValue(value)) {
      return Chartist.getNumberOrUndefined(value[dimension || 'y']);
    } else {
      return Chartist.getNumberOrUndefined(value);
    }
  };

  /**
   * Pollard Rho Algorithm to find smallest factor of an integer value. There are more efficient algorithms for factorization, but this one is quite efficient and not so complex.
   *
   * @memberof Chartist.Core
   * @param {Number} num An integer number where the smallest factor should be searched for
   * @returns {Number} The smallest integer factor of the parameter num.
   */
  Chartist.rho = function(num) {
    if(num === 1) {
      return num;
    }

    function gcd(p, q) {
      if (p % q === 0) {
        return q;
      } else {
        return gcd(q, p % q);
      }
    }

    function f(x) {
      return x * x + 1;
    }

    var x1 = 2, x2 = 2, divisor;
    if (num % 2 === 0) {
      return 2;
    }

    do {
      x1 = f(x1) % num;
      x2 = f(f(x2)) % num;
      divisor = gcd(Math.abs(x1 - x2), num);
    } while (divisor === 1);

    return divisor;
  };

  /**
   * Calculate and retrieve all the bounds for the chart and return them in one array
   *
   * @memberof Chartist.Core
   * @param {Number} axisLength The length of the Axis used for
   * @param {Object} highLow An object containing a high and low property indicating the value range of the chart.
   * @param {Number} scaleMinSpace The minimum projected length a step should result in
   * @param {Boolean} onlyInteger
   * @return {Object} All the values to set the bounds of the chart
   */
  Chartist.getBounds = function (axisLength, highLow, scaleMinSpace, onlyInteger) {
    var i,
      optimizationCounter = 0,
      newMin,
      newMax,
      bounds = {
        high: highLow.high,
        low: highLow.low
      };

    bounds.valueRange = bounds.high - bounds.low;
    bounds.oom = Chartist.orderOfMagnitude(bounds.valueRange);
    bounds.step = Math.pow(10, bounds.oom);
    bounds.min = Math.floor(bounds.low / bounds.step) * bounds.step;
    bounds.max = Math.ceil(bounds.high / bounds.step) * bounds.step;
    bounds.range = bounds.max - bounds.min;
    bounds.numberOfSteps = Math.round(bounds.range / bounds.step);

    // Optimize scale step by checking if subdivision is possible based on horizontalGridMinSpace
    // If we are already below the scaleMinSpace value we will scale up
    var length = Chartist.projectLength(axisLength, bounds.step, bounds);
    var scaleUp = length < scaleMinSpace;
    var smallestFactor = onlyInteger ? Chartist.rho(bounds.range) : 0;

    // First check if we should only use integer steps and if step 1 is still larger than scaleMinSpace so we can use 1
    if(onlyInteger && Chartist.projectLength(axisLength, 1, bounds) >= scaleMinSpace) {
      bounds.step = 1;
    } else if(onlyInteger && smallestFactor < bounds.step && Chartist.projectLength(axisLength, smallestFactor, bounds) >= scaleMinSpace) {
      // If step 1 was too small, we can try the smallest factor of range
      // If the smallest factor is smaller than the current bounds.step and the projected length of smallest factor
      // is larger than the scaleMinSpace we should go for it.
      bounds.step = smallestFactor;
    } else {
      // Trying to divide or multiply by 2 and find the best step value
      while (true) {
        if (scaleUp && Chartist.projectLength(axisLength, bounds.step, bounds) <= scaleMinSpace) {
          bounds.step *= 2;
        } else if (!scaleUp && Chartist.projectLength(axisLength, bounds.step / 2, bounds) >= scaleMinSpace) {
          bounds.step /= 2;
          if(onlyInteger && bounds.step % 1 !== 0) {
            bounds.step *= 2;
            break;
          }
        } else {
          break;
        }

        if(optimizationCounter++ > 1000) {
          throw new Error('Exceeded maximum number of iterations while optimizing scale step!');
        }
      }
    }

    var EPSILON = 2.221E-16;
    bounds.step = Math.max(bounds.step, EPSILON);
    function safeIncrement(value, increment) {
      // If increment is too small use *= (1+EPSILON) as a simple nextafter
      if (value === (value += increment)) {
      	value *= (1 + (increment > 0 ? EPSILON : -EPSILON));
      }
      return value;
    }

    // Narrow min and max based on new step
    newMin = bounds.min;
    newMax = bounds.max;
    while (newMin + bounds.step <= bounds.low) {
    	newMin = safeIncrement(newMin, bounds.step);
    }
    while (newMax - bounds.step >= bounds.high) {
    	newMax = safeIncrement(newMax, -bounds.step);
    }
    bounds.min = newMin;
    bounds.max = newMax;
    bounds.range = bounds.max - bounds.min;

    var values = [];
    for (i = bounds.min; i <= bounds.max; i = safeIncrement(i, bounds.step)) {
      var value = Chartist.roundWithPrecision(i);
      if (value !== values[values.length - 1]) {
        values.push(value);
      }
    }
    bounds.values = values;
    return bounds;
  };

  /**
   * Calculate cartesian coordinates of polar coordinates
   *
   * @memberof Chartist.Core
   * @param {Number} centerX X-axis coordinates of center point of circle segment
   * @param {Number} centerY X-axis coordinates of center point of circle segment
   * @param {Number} radius Radius of circle segment
   * @param {Number} angleInDegrees Angle of circle segment in degrees
   * @return {{x:Number, y:Number}} Coordinates of point on circumference
   */
  Chartist.polarToCartesian = function (centerX, centerY, radius, angleInDegrees) {
    var angleInRadians = (angleInDegrees - 90) * Math.PI / 180.0;

    return {
      x: centerX + (radius * Math.cos(angleInRadians)),
      y: centerY + (radius * Math.sin(angleInRadians))
    };
  };

  /**
   * Initialize chart drawing rectangle (area where chart is drawn) x1,y1 = bottom left / x2,y2 = top right
   *
   * @memberof Chartist.Core
   * @param {Object} svg The svg element for the chart
   * @param {Object} options The Object that contains all the optional values for the chart
   * @param {Number} [fallbackPadding] The fallback padding if partial padding objects are used
   * @return {Object} The chart rectangles coordinates inside the svg element plus the rectangles measurements
   */
  Chartist.createChartRect = function (svg, options, fallbackPadding) {
    var hasAxis = !!(options.axisX || options.axisY);
    var yAxisOffset = hasAxis ? options.axisY.offset : 0;
    var xAxisOffset = hasAxis ? options.axisX.offset : 0;
    // If width or height results in invalid value (including 0) we fallback to the unitless settings or even 0
    var width = svg.width() || Chartist.quantity(options.width).value || 0;
    var height = svg.height() || Chartist.quantity(options.height).value || 0;
    var normalizedPadding = Chartist.normalizePadding(options.chartPadding, fallbackPadding);

    // If settings were to small to cope with offset (legacy) and padding, we'll adjust
    width = Math.max(width, yAxisOffset + normalizedPadding.left + normalizedPadding.right);
    height = Math.max(height, xAxisOffset + normalizedPadding.top + normalizedPadding.bottom);

    var chartRect = {
      padding: normalizedPadding,
      width: function () {
        return this.x2 - this.x1;
      },
      height: function () {
        return this.y1 - this.y2;
      }
    };

    if(hasAxis) {
      if (options.axisX.position === 'start') {
        chartRect.y2 = normalizedPadding.top + xAxisOffset;
        chartRect.y1 = Math.max(height - normalizedPadding.bottom, chartRect.y2 + 1);
      } else {
        chartRect.y2 = normalizedPadding.top;
        chartRect.y1 = Math.max(height - normalizedPadding.bottom - xAxisOffset, chartRect.y2 + 1);
      }

      if (options.axisY.position === 'start') {
        chartRect.x1 = normalizedPadding.left + yAxisOffset;
        chartRect.x2 = Math.max(width - normalizedPadding.right, chartRect.x1 + 1);
      } else {
        chartRect.x1 = normalizedPadding.left;
        chartRect.x2 = Math.max(width - normalizedPadding.right - yAxisOffset, chartRect.x1 + 1);
      }
    } else {
      chartRect.x1 = normalizedPadding.left;
      chartRect.x2 = Math.max(width - normalizedPadding.right, chartRect.x1 + 1);
      chartRect.y2 = normalizedPadding.top;
      chartRect.y1 = Math.max(height - normalizedPadding.bottom, chartRect.y2 + 1);
    }

    return chartRect;
  };

  /**
   * Creates a grid line based on a projected value.
   *
   * @memberof Chartist.Core
   * @param position
   * @param index
   * @param axis
   * @param offset
   * @param length
   * @param group
   * @param classes
   * @param eventEmitter
   */
  Chartist.createGrid = function(position, index, axis, offset, length, group, classes, eventEmitter) {
    var positionalData = {};
    positionalData[axis.units.pos + '1'] = position;
    positionalData[axis.units.pos + '2'] = position;
    positionalData[axis.counterUnits.pos + '1'] = offset;
    positionalData[axis.counterUnits.pos + '2'] = offset + length;

    var gridElement = group.elem('line', positionalData, classes.join(' '));

    // Event for grid draw
    eventEmitter.emit('draw',
      Chartist.extend({
        type: 'grid',
        axis: axis,
        index: index,
        group: group,
        element: gridElement
      }, positionalData)
    );
  };

  /**
   * Creates a grid background rect and emits the draw event.
   *
   * @memberof Chartist.Core
   * @param gridGroup
   * @param chartRect
   * @param className
   * @param eventEmitter
   */
  Chartist.createGridBackground = function (gridGroup, chartRect, className, eventEmitter) {
    var gridBackground = gridGroup.elem('rect', {
        x: chartRect.x1,
        y: chartRect.y2,
        width: chartRect.width(),
        height: chartRect.height(),
      }, className, true);

      // Event for grid background draw
      eventEmitter.emit('draw', {
        type: 'gridBackground',
        group: gridGroup,
        element: gridBackground
      });
  };

  /**
   * Creates a label based on a projected value and an axis.
   *
   * @memberof Chartist.Core
   * @param position
   * @param length
   * @param index
   * @param labels
   * @param axis
   * @param axisOffset
   * @param labelOffset
   * @param group
   * @param classes
   * @param useForeignObject
   * @param eventEmitter
   */
  Chartist.createLabel = function(position, length, index, labels, axis, axisOffset, labelOffset, group, classes, useForeignObject, eventEmitter) {
    var labelElement;
    var positionalData = {};

    positionalData[axis.units.pos] = position + labelOffset[axis.units.pos];
    positionalData[axis.counterUnits.pos] = labelOffset[axis.counterUnits.pos];
    positionalData[axis.units.len] = length;
    positionalData[axis.counterUnits.len] = Math.max(0, axisOffset - 10);

    if(useForeignObject) {
      // We need to set width and height explicitly to px as span will not expand with width and height being
      // 100% in all browsers
      var content = document.createElement('span');
      content.className = classes.join(' ');
      content.setAttribute('xmlns', Chartist.namespaces.xhtml);
      content.innerText = labels[index];
      content.style[axis.units.len] = Math.round(positionalData[axis.units.len]) + 'px';
      content.style[axis.counterUnits.len] = Math.round(positionalData[axis.counterUnits.len]) + 'px';

      labelElement = group.foreignObject(content, Chartist.extend({
        style: 'overflow: visible;'
      }, positionalData));
    } else {
      labelElement = group.elem('text', positionalData, classes.join(' ')).text(labels[index]);
    }

    eventEmitter.emit('draw', Chartist.extend({
      type: 'label',
      axis: axis,
      index: index,
      group: group,
      element: labelElement,
      text: labels[index]
    }, positionalData));
  };

  /**
   * Helper to read series specific options from options object. It automatically falls back to the global option if
   * there is no option in the series options.
   *
   * @param {Object} series Series object
   * @param {Object} options Chartist options object
   * @param {string} key The options key that should be used to obtain the options
   * @returns {*}
   */
  Chartist.getSeriesOption = function(series, options, key) {
    if(series.name && options.series && options.series[series.name]) {
      var seriesOptions = options.series[series.name];
      return seriesOptions.hasOwnProperty(key) ? seriesOptions[key] : options[key];
    } else {
      return options[key];
    }
  };

  /**
   * Provides options handling functionality with callback for options changes triggered by responsive options and media query matches
   *
   * @memberof Chartist.Core
   * @param {Object} options Options set by user
   * @param {Array} responsiveOptions Optional functions to add responsive behavior to chart
   * @param {Object} eventEmitter The event emitter that will be used to emit the options changed events
   * @return {Object} The consolidated options object from the defaults, base and matching responsive options
   */
  Chartist.optionsProvider = function (options, responsiveOptions, eventEmitter) {
    var baseOptions = Chartist.extend({}, options),
      currentOptions,
      mediaQueryListeners = [],
      i;

    function updateCurrentOptions(mediaEvent) {
      var previousOptions = currentOptions;
      currentOptions = Chartist.extend({}, baseOptions);

      if (responsiveOptions) {
        for (i = 0; i < responsiveOptions.length; i++) {
          var mql = window.matchMedia(responsiveOptions[i][0]);
          if (mql.matches) {
            currentOptions = Chartist.extend(currentOptions, responsiveOptions[i][1]);
          }
        }
      }

      if(eventEmitter && mediaEvent) {
        eventEmitter.emit('optionsChanged', {
          previousOptions: previousOptions,
          currentOptions: currentOptions
        });
      }
    }

    function removeMediaQueryListeners() {
      mediaQueryListeners.forEach(function(mql) {
        mql.removeListener(updateCurrentOptions);
      });
    }

    if (!window.matchMedia) {
      throw 'window.matchMedia not found! Make sure you\'re using a polyfill.';
    } else if (responsiveOptions) {

      for (i = 0; i < responsiveOptions.length; i++) {
        var mql = window.matchMedia(responsiveOptions[i][0]);
        mql.addListener(updateCurrentOptions);
        mediaQueryListeners.push(mql);
      }
    }
    // Execute initially without an event argument so we get the correct options
    updateCurrentOptions();

    return {
      removeMediaQueryListeners: removeMediaQueryListeners,
      getCurrentOptions: function getCurrentOptions() {
        return Chartist.extend({}, currentOptions);
      }
    };
  };


  /**
   * Splits a list of coordinates and associated values into segments. Each returned segment contains a pathCoordinates
   * valueData property describing the segment.
   *
   * With the default options, segments consist of contiguous sets of points that do not have an undefined value. Any
   * points with undefined values are discarded.
   *
   * **Options**
   * The following options are used to determine how segments are formed
   * ```javascript
   * var options = {
   *   // If fillHoles is true, undefined values are simply discarded without creating a new segment. Assuming other options are default, this returns single segment.
   *   fillHoles: false,
   *   // If increasingX is true, the coordinates in all segments have strictly increasing x-values.
   *   increasingX: false
   * };
   * ```
   *
   * @memberof Chartist.Core
   * @param {Array} pathCoordinates List of point coordinates to be split in the form [x1, y1, x2, y2 ... xn, yn]
   * @param {Array} values List of associated point values in the form [v1, v2 .. vn]
   * @param {Object} options Options set by user
   * @return {Array} List of segments, each containing a pathCoordinates and valueData property.
   */
  Chartist.splitIntoSegments = function(pathCoordinates, valueData, options) {
    var defaultOptions = {
      increasingX: false,
      fillHoles: false
    };

    options = Chartist.extend({}, defaultOptions, options);

    var segments = [];
    var hole = true;

    for(var i = 0; i < pathCoordinates.length; i += 2) {
      // If this value is a "hole" we set the hole flag
      if(Chartist.getMultiValue(valueData[i / 2].value) === undefined) {
      // if(valueData[i / 2].value === undefined) {
        if(!options.fillHoles) {
          hole = true;
        }
      } else {
        if(options.increasingX && i >= 2 && pathCoordinates[i] <= pathCoordinates[i-2]) {
          // X is not increasing, so we need to make sure we start a new segment
          hole = true;
        }


        // If it's a valid value we need to check if we're coming out of a hole and create a new empty segment
        if(hole) {
          segments.push({
            pathCoordinates: [],
            valueData: []
          });
          // As we have a valid value now, we are not in a "hole" anymore
          hole = false;
        }

        // Add to the segment pathCoordinates and valueData
        segments[segments.length - 1].pathCoordinates.push(pathCoordinates[i], pathCoordinates[i + 1]);
        segments[segments.length - 1].valueData.push(valueData[i / 2]);
      }
    }

    return segments;
  };
}(window, document, Chartist));
;/**
 * Chartist path interpolation functions.
 *
 * @module Chartist.Interpolation
 */
/* global Chartist */
(function(window, document, Chartist) {
  'use strict';

  Chartist.Interpolation = {};

  /**
   * This interpolation function does not smooth the path and the result is only containing lines and no curves.
   *
   * @example
   * var chart = new Chartist.Line('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5],
   *   series: [[1, 2, 8, 1, 7]]
   * }, {
   *   lineSmooth: Chartist.Interpolation.none({
   *     fillHoles: false
   *   })
   * });
   *
   *
   * @memberof Chartist.Interpolation
   * @return {Function}
   */
  Chartist.Interpolation.none = function(options) {
    var defaultOptions = {
      fillHoles: false
    };
    options = Chartist.extend({}, defaultOptions, options);
    return function none(pathCoordinates, valueData) {
      var path = new Chartist.Svg.Path();
      var hole = true;

      for(var i = 0; i < pathCoordinates.length; i += 2) {
        var currX = pathCoordinates[i];
        var currY = pathCoordinates[i + 1];
        var currData = valueData[i / 2];

        if(Chartist.getMultiValue(currData.value) !== undefined) {

          if(hole) {
            path.move(currX, currY, false, currData);
          } else {
            path.line(currX, currY, false, currData);
          }

          hole = false;
        } else if(!options.fillHoles) {
          hole = true;
        }
      }

      return path;
    };
  };

  /**
   * Simple smoothing creates horizontal handles that are positioned with a fraction of the length between two data points. You can use the divisor option to specify the amount of smoothing.
   *
   * Simple smoothing can be used instead of `Chartist.Smoothing.cardinal` if you'd like to get rid of the artifacts it produces sometimes. Simple smoothing produces less flowing lines but is accurate by hitting the points and it also doesn't swing below or above the given data point.
   *
   * All smoothing functions within Chartist are factory functions that accept an options parameter. The simple interpolation function accepts one configuration parameter `divisor`, between 1 and ∞, which controls the smoothing characteristics.
   *
   * @example
   * var chart = new Chartist.Line('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5],
   *   series: [[1, 2, 8, 1, 7]]
   * }, {
   *   lineSmooth: Chartist.Interpolation.simple({
   *     divisor: 2,
   *     fillHoles: false
   *   })
   * });
   *
   *
   * @memberof Chartist.Interpolation
   * @param {Object} options The options of the simple interpolation factory function.
   * @return {Function}
   */
  Chartist.Interpolation.simple = function(options) {
    var defaultOptions = {
      divisor: 2,
      fillHoles: false
    };
    options = Chartist.extend({}, defaultOptions, options);

    var d = 1 / Math.max(1, options.divisor);

    return function simple(pathCoordinates, valueData) {
      var path = new Chartist.Svg.Path();
      var prevX, prevY, prevData;

      for(var i = 0; i < pathCoordinates.length; i += 2) {
        var currX = pathCoordinates[i];
        var currY = pathCoordinates[i + 1];
        var length = (currX - prevX) * d;
        var currData = valueData[i / 2];

        if(currData.value !== undefined) {

          if(prevData === undefined) {
            path.move(currX, currY, false, currData);
          } else {
            path.curve(
              prevX + length,
              prevY,
              currX - length,
              currY,
              currX,
              currY,
              false,
              currData
            );
          }

          prevX = currX;
          prevY = currY;
          prevData = currData;
        } else if(!options.fillHoles) {
          prevX = currX = prevData = undefined;
        }
      }

      return path;
    };
  };

  /**
   * Cardinal / Catmull-Rome spline interpolation is the default smoothing function in Chartist. It produces nice results where the splines will always meet the points. It produces some artifacts though when data values are increased or decreased rapidly. The line may not follow a very accurate path and if the line should be accurate this smoothing function does not produce the best results.
   *
   * Cardinal splines can only be created if there are more than two data points. If this is not the case this smoothing will fallback to `Chartist.Smoothing.none`.
   *
   * All smoothing functions within Chartist are factory functions that accept an options parameter. The cardinal interpolation function accepts one configuration parameter `tension`, between 0 and 1, which controls the smoothing intensity.
   *
   * @example
   * var chart = new Chartist.Line('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5],
   *   series: [[1, 2, 8, 1, 7]]
   * }, {
   *   lineSmooth: Chartist.Interpolation.cardinal({
   *     tension: 1,
   *     fillHoles: false
   *   })
   * });
   *
   * @memberof Chartist.Interpolation
   * @param {Object} options The options of the cardinal factory function.
   * @return {Function}
   */
  Chartist.Interpolation.cardinal = function(options) {
    var defaultOptions = {
      tension: 1,
      fillHoles: false
    };

    options = Chartist.extend({}, defaultOptions, options);

    var t = Math.min(1, Math.max(0, options.tension)),
      c = 1 - t;

    return function cardinal(pathCoordinates, valueData) {
      // First we try to split the coordinates into segments
      // This is necessary to treat "holes" in line charts
      var segments = Chartist.splitIntoSegments(pathCoordinates, valueData, {
        fillHoles: options.fillHoles
      });

      if(!segments.length) {
        // If there were no segments return 'Chartist.Interpolation.none'
        return Chartist.Interpolation.none()([]);
      } else if(segments.length > 1) {
        // If the split resulted in more that one segment we need to interpolate each segment individually and join them
        // afterwards together into a single path.
          var paths = [];
        // For each segment we will recurse the cardinal function
        segments.forEach(function(segment) {
          paths.push(cardinal(segment.pathCoordinates, segment.valueData));
        });
        // Join the segment path data into a single path and return
        return Chartist.Svg.Path.join(paths);
      } else {
        // If there was only one segment we can proceed regularly by using pathCoordinates and valueData from the first
        // segment
        pathCoordinates = segments[0].pathCoordinates;
        valueData = segments[0].valueData;

        // If less than two points we need to fallback to no smoothing
        if(pathCoordinates.length <= 4) {
          return Chartist.Interpolation.none()(pathCoordinates, valueData);
        }

        var path = new Chartist.Svg.Path().move(pathCoordinates[0], pathCoordinates[1], false, valueData[0]),
          z;

        for (var i = 0, iLen = pathCoordinates.length; iLen - 2 * !z > i; i += 2) {
          var p = [
            {x: +pathCoordinates[i - 2], y: +pathCoordinates[i - 1]},
            {x: +pathCoordinates[i], y: +pathCoordinates[i + 1]},
            {x: +pathCoordinates[i + 2], y: +pathCoordinates[i + 3]},
            {x: +pathCoordinates[i + 4], y: +pathCoordinates[i + 5]}
          ];
          if (z) {
            if (!i) {
              p[0] = {x: +pathCoordinates[iLen - 2], y: +pathCoordinates[iLen - 1]};
            } else if (iLen - 4 === i) {
              p[3] = {x: +pathCoordinates[0], y: +pathCoordinates[1]};
            } else if (iLen - 2 === i) {
              p[2] = {x: +pathCoordinates[0], y: +pathCoordinates[1]};
              p[3] = {x: +pathCoordinates[2], y: +pathCoordinates[3]};
            }
          } else {
            if (iLen - 4 === i) {
              p[3] = p[2];
            } else if (!i) {
              p[0] = {x: +pathCoordinates[i], y: +pathCoordinates[i + 1]};
            }
          }

          path.curve(
            (t * (-p[0].x + 6 * p[1].x + p[2].x) / 6) + (c * p[2].x),
            (t * (-p[0].y + 6 * p[1].y + p[2].y) / 6) + (c * p[2].y),
            (t * (p[1].x + 6 * p[2].x - p[3].x) / 6) + (c * p[2].x),
            (t * (p[1].y + 6 * p[2].y - p[3].y) / 6) + (c * p[2].y),
            p[2].x,
            p[2].y,
            false,
            valueData[(i + 2) / 2]
          );
        }

        return path;
      }
    };
  };

  /**
   * Monotone Cubic spline interpolation produces a smooth curve which preserves monotonicity. Unlike cardinal splines, the curve will not extend beyond the range of y-values of the original data points.
   *
   * Monotone Cubic splines can only be created if there are more than two data points. If this is not the case this smoothing will fallback to `Chartist.Smoothing.none`.
   *
   * The x-values of subsequent points must be increasing to fit a Monotone Cubic spline. If this condition is not met for a pair of adjacent points, then there will be a break in the curve between those data points.
   *
   * All smoothing functions within Chartist are factory functions that accept an options parameter.
   *
   * @example
   * var chart = new Chartist.Line('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5],
   *   series: [[1, 2, 8, 1, 7]]
   * }, {
   *   lineSmooth: Chartist.Interpolation.monotoneCubic({
   *     fillHoles: false
   *   })
   * });
   *
   * @memberof Chartist.Interpolation
   * @param {Object} options The options of the monotoneCubic factory function.
   * @return {Function}
   */
  Chartist.Interpolation.monotoneCubic = function(options) {
    var defaultOptions = {
      fillHoles: false
    };

    options = Chartist.extend({}, defaultOptions, options);

    return function monotoneCubic(pathCoordinates, valueData) {
      // First we try to split the coordinates into segments
      // This is necessary to treat "holes" in line charts
      var segments = Chartist.splitIntoSegments(pathCoordinates, valueData, {
        fillHoles: options.fillHoles,
        increasingX: true
      });

      if(!segments.length) {
        // If there were no segments return 'Chartist.Interpolation.none'
        return Chartist.Interpolation.none()([]);
      } else if(segments.length > 1) {
        // If the split resulted in more that one segment we need to interpolate each segment individually and join them
        // afterwards together into a single path.
          var paths = [];
        // For each segment we will recurse the monotoneCubic fn function
        segments.forEach(function(segment) {
          paths.push(monotoneCubic(segment.pathCoordinates, segment.valueData));
        });
        // Join the segment path data into a single path and return
        return Chartist.Svg.Path.join(paths);
      } else {
        // If there was only one segment we can proceed regularly by using pathCoordinates and valueData from the first
        // segment
        pathCoordinates = segments[0].pathCoordinates;
        valueData = segments[0].valueData;

        // If less than three points we need to fallback to no smoothing
        if(pathCoordinates.length <= 4) {
          return Chartist.Interpolation.none()(pathCoordinates, valueData);
        }

        var xs = [],
          ys = [],
          i,
          n = pathCoordinates.length / 2,
          ms = [],
          ds = [], dys = [], dxs = [],
          path;

        // Populate x and y coordinates into separate arrays, for readability

        for(i = 0; i < n; i++) {
          xs[i] = pathCoordinates[i * 2];
          ys[i] = pathCoordinates[i * 2 + 1];
        }

        // Calculate deltas and derivative

        for(i = 0; i < n - 1; i++) {
          dys[i] = ys[i + 1] - ys[i];
          dxs[i] = xs[i + 1] - xs[i];
          ds[i] = dys[i] / dxs[i];
        }

        // Determine desired slope (m) at each point using Fritsch-Carlson method
        // See: http://math.stackexchange.com/questions/45218/implementation-of-monotone-cubic-interpolation

        ms[0] = ds[0];
        ms[n - 1] = ds[n - 2];

        for(i = 1; i < n - 1; i++) {
          if(ds[i] === 0 || ds[i - 1] === 0 || (ds[i - 1] > 0) !== (ds[i] > 0)) {
            ms[i] = 0;
          } else {
            ms[i] = 3 * (dxs[i - 1] + dxs[i]) / (
              (2 * dxs[i] + dxs[i - 1]) / ds[i - 1] +
              (dxs[i] + 2 * dxs[i - 1]) / ds[i]);

            if(!isFinite(ms[i])) {
              ms[i] = 0;
            }
          }
        }

        // Now build a path from the slopes

        path = new Chartist.Svg.Path().move(xs[0], ys[0], false, valueData[0]);

        for(i = 0; i < n - 1; i++) {
          path.curve(
            // First control point
            xs[i] + dxs[i] / 3,
            ys[i] + ms[i] * dxs[i] / 3,
            // Second control point
            xs[i + 1] - dxs[i] / 3,
            ys[i + 1] - ms[i + 1] * dxs[i] / 3,
            // End point
            xs[i + 1],
            ys[i + 1],

            false,
            valueData[i + 1]
          );
        }

        return path;
      }
    };
  };

  /**
   * Step interpolation will cause the line chart to move in steps rather than diagonal or smoothed lines. This interpolation will create additional points that will also be drawn when the `showPoint` option is enabled.
   *
   * All smoothing functions within Chartist are factory functions that accept an options parameter. The step interpolation function accepts one configuration parameter `postpone`, that can be `true` or `false`. The default value is `true` and will cause the step to occur where the value actually changes. If a different behaviour is needed where the step is shifted to the left and happens before the actual value, this option can be set to `false`.
   *
   * @example
   * var chart = new Chartist.Line('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5],
   *   series: [[1, 2, 8, 1, 7]]
   * }, {
   *   lineSmooth: Chartist.Interpolation.step({
   *     postpone: true,
   *     fillHoles: false
   *   })
   * });
   *
   * @memberof Chartist.Interpolation
   * @param options
   * @returns {Function}
   */
  Chartist.Interpolation.step = function(options) {
    var defaultOptions = {
      postpone: true,
      fillHoles: false
    };

    options = Chartist.extend({}, defaultOptions, options);

    return function step(pathCoordinates, valueData) {
      var path = new Chartist.Svg.Path();

      var prevX, prevY, prevData;

      for (var i = 0; i < pathCoordinates.length; i += 2) {
        var currX = pathCoordinates[i];
        var currY = pathCoordinates[i + 1];
        var currData = valueData[i / 2];

        // If the current point is also not a hole we can draw the step lines
        if(currData.value !== undefined) {
          if(prevData === undefined) {
            path.move(currX, currY, false, currData);
          } else {
            if(options.postpone) {
              // If postponed we should draw the step line with the value of the previous value
              path.line(currX, prevY, false, prevData);
            } else {
              // If not postponed we should draw the step line with the value of the current value
              path.line(prevX, currY, false, currData);
            }
            // Line to the actual point (this should only be a Y-Axis movement
            path.line(currX, currY, false, currData);
          }

          prevX = currX;
          prevY = currY;
          prevData = currData;
        } else if(!options.fillHoles) {
          prevX = prevY = prevData = undefined;
        }
      }

      return path;
    };
  };

}(window, document, Chartist));
;/**
 * A very basic event module that helps to generate and catch events.
 *
 * @module Chartist.Event
 */
/* global Chartist */
(function (window, document, Chartist) {
  'use strict';

  Chartist.EventEmitter = function () {
    var handlers = [];

    /**
     * Add an event handler for a specific event
     *
     * @memberof Chartist.Event
     * @param {String} event The event name
     * @param {Function} handler A event handler function
     */
    function addEventHandler(event, handler) {
      handlers[event] = handlers[event] || [];
      handlers[event].push(handler);
    }

    /**
     * Remove an event handler of a specific event name or remove all event handlers for a specific event.
     *
     * @memberof Chartist.Event
     * @param {String} event The event name where a specific or all handlers should be removed
     * @param {Function} [handler] An optional event handler function. If specified only this specific handler will be removed and otherwise all handlers are removed.
     */
    function removeEventHandler(event, handler) {
      // Only do something if there are event handlers with this name existing
      if(handlers[event]) {
        // If handler is set we will look for a specific handler and only remove this
        if(handler) {
          handlers[event].splice(handlers[event].indexOf(handler), 1);
          if(handlers[event].length === 0) {
            delete handlers[event];
          }
        } else {
          // If no handler is specified we remove all handlers for this event
          delete handlers[event];
        }
      }
    }

    /**
     * Use this function to emit an event. All handlers that are listening for this event will be triggered with the data parameter.
     *
     * @memberof Chartist.Event
     * @param {String} event The event name that should be triggered
     * @param {*} data Arbitrary data that will be passed to the event handler callback functions
     */
    function emit(event, data) {
      // Only do something if there are event handlers with this name existing
      if(handlers[event]) {
        handlers[event].forEach(function(handler) {
          handler(data);
        });
      }

      // Emit event to star event handlers
      if(handlers['*']) {
        handlers['*'].forEach(function(starHandler) {
          starHandler(event, data);
        });
      }
    }

    return {
      addEventHandler: addEventHandler,
      removeEventHandler: removeEventHandler,
      emit: emit
    };
  };

}(window, document, Chartist));
;/**
 * This module provides some basic prototype inheritance utilities.
 *
 * @module Chartist.Class
 */
/* global Chartist */
(function(window, document, Chartist) {
  'use strict';

  function listToArray(list) {
    var arr = [];
    if (list.length) {
      for (var i = 0; i < list.length; i++) {
        arr.push(list[i]);
      }
    }
    return arr;
  }

  /**
   * Method to extend from current prototype.
   *
   * @memberof Chartist.Class
   * @param {Object} properties The object that serves as definition for the prototype that gets created for the new class. This object should always contain a constructor property that is the desired constructor for the newly created class.
   * @param {Object} [superProtoOverride] By default extens will use the current class prototype or Chartist.class. With this parameter you can specify any super prototype that will be used.
   * @return {Function} Constructor function of the new class
   *
   * @example
   * var Fruit = Class.extend({
     * color: undefined,
     *   sugar: undefined,
     *
     *   constructor: function(color, sugar) {
     *     this.color = color;
     *     this.sugar = sugar;
     *   },
     *
     *   eat: function() {
     *     this.sugar = 0;
     *     return this;
     *   }
     * });
   *
   * var Banana = Fruit.extend({
     *   length: undefined,
     *
     *   constructor: function(length, sugar) {
     *     Banana.super.constructor.call(this, 'Yellow', sugar);
     *     this.length = length;
     *   }
     * });
   *
   * var banana = new Banana(20, 40);
   * console.log('banana instanceof Fruit', banana instanceof Fruit);
   * console.log('Fruit is prototype of banana', Fruit.prototype.isPrototypeOf(banana));
   * console.log('bananas prototype is Fruit', Object.getPrototypeOf(banana) === Fruit.prototype);
   * console.log(banana.sugar);
   * console.log(banana.eat().sugar);
   * console.log(banana.color);
   */
  function extend(properties, superProtoOverride) {
    var superProto = superProtoOverride || this.prototype || Chartist.Class;
    var proto = Object.create(superProto);

    Chartist.Class.cloneDefinitions(proto, properties);

    var constr = function() {
      var fn = proto.constructor || function () {},
        instance;

      // If this is linked to the Chartist namespace the constructor was not called with new
      // To provide a fallback we will instantiate here and return the instance
      instance = this === Chartist ? Object.create(proto) : this;
      fn.apply(instance, Array.prototype.slice.call(arguments, 0));

      // If this constructor was not called with new we need to return the instance
      // This will not harm when the constructor has been called with new as the returned value is ignored
      return instance;
    };

    constr.prototype = proto;
    constr.super = superProto;
    constr.extend = this.extend;

    return constr;
  }

  // Variable argument list clones args > 0 into args[0] and retruns modified args[0]
  function cloneDefinitions() {
    var args = listToArray(arguments);
    var target = args[0];

    args.splice(1, args.length - 1).forEach(function (source) {
      Object.getOwnPropertyNames(source).forEach(function (propName) {
        // If this property already exist in target we delete it first
        delete target[propName];
        // Define the property with the descriptor from source
        Object.defineProperty(target, propName,
          Object.getOwnPropertyDescriptor(source, propName));
      });
    });

    return target;
  }

  Chartist.Class = {
    extend: extend,
    cloneDefinitions: cloneDefinitions
  };

}(window, document, Chartist));
;/**
 * Base for all chart types. The methods in Chartist.Base are inherited to all chart types.
 *
 * @module Chartist.Base
 */
/* global Chartist */
(function(window, document, Chartist) {
  'use strict';

  // TODO: Currently we need to re-draw the chart on window resize. This is usually very bad and will affect performance.
  // This is done because we can't work with relative coordinates when drawing the chart because SVG Path does not
  // work with relative positions yet. We need to check if we can do a viewBox hack to switch to percentage.
  // See http://mozilla.6506.n7.nabble.com/Specyfing-paths-with-percentages-unit-td247474.html
  // Update: can be done using the above method tested here: http://codepen.io/gionkunz/pen/KDvLj
  // The problem is with the label offsets that can't be converted into percentage and affecting the chart container
  /**
   * Updates the chart which currently does a full reconstruction of the SVG DOM
   *
   * @param {Object} [data] Optional data you'd like to set for the chart before it will update. If not specified the update method will use the data that is already configured with the chart.
   * @param {Object} [options] Optional options you'd like to add to the previous options for the chart before it will update. If not specified the update method will use the options that have been already configured with the chart.
   * @param {Boolean} [override] If set to true, the passed options will be used to extend the options that have been configured already. Otherwise the chart default options will be used as the base
   * @memberof Chartist.Base
   */
  function update(data, options, override) {
    if(data) {
      this.data = data || {};
      this.data.labels = this.data.labels || [];
      this.data.series = this.data.series || [];
      // Event for data transformation that allows to manipulate the data before it gets rendered in the charts
      this.eventEmitter.emit('data', {
        type: 'update',
        data: this.data
      });
    }

    if(options) {
      this.options = Chartist.extend({}, override ? this.options : this.defaultOptions, options);

      // If chartist was not initialized yet, we just set the options and leave the rest to the initialization
      // Otherwise we re-create the optionsProvider at this point
      if(!this.initializeTimeoutId) {
        this.optionsProvider.removeMediaQueryListeners();
        this.optionsProvider = Chartist.optionsProvider(this.options, this.responsiveOptions, this.eventEmitter);
      }
    }

    // Only re-created the chart if it has been initialized yet
    if(!this.initializeTimeoutId) {
      this.createChart(this.optionsProvider.getCurrentOptions());
    }

    // Return a reference to the chart object to chain up calls
    return this;
  }

  /**
   * This method can be called on the API object of each chart and will un-register all event listeners that were added to other components. This currently includes a window.resize listener as well as media query listeners if any responsive options have been provided. Use this function if you need to destroy and recreate Chartist charts dynamically.
   *
   * @memberof Chartist.Base
   */
  function detach() {
    // Only detach if initialization already occurred on this chart. If this chart still hasn't initialized (therefore
    // the initializationTimeoutId is still a valid timeout reference, we will clear the timeout
    if(!this.initializeTimeoutId) {
      window.removeEventListener('resize', this.resizeListener);
      this.optionsProvider.removeMediaQueryListeners();
    } else {
      window.clearTimeout(this.initializeTimeoutId);
    }

    return this;
  }

  /**
   * Use this function to register event handlers. The handler callbacks are synchronous and will run in the main thread rather than the event loop.
   *
   * @memberof Chartist.Base
   * @param {String} event Name of the event. Check the examples for supported events.
   * @param {Function} handler The handler function that will be called when an event with the given name was emitted. This function will receive a data argument which contains event data. See the example for more details.
   */
  function on(event, handler) {
    this.eventEmitter.addEventHandler(event, handler);
    return this;
  }

  /**
   * Use this function to un-register event handlers. If the handler function parameter is omitted all handlers for the given event will be un-registered.
   *
   * @memberof Chartist.Base
   * @param {String} event Name of the event for which a handler should be removed
   * @param {Function} [handler] The handler function that that was previously used to register a new event handler. This handler will be removed from the event handler list. If this parameter is omitted then all event handlers for the given event are removed from the list.
   */
  function off(event, handler) {
    this.eventEmitter.removeEventHandler(event, handler);
    return this;
  }

  function initialize() {
    // Add window resize listener that re-creates the chart
    window.addEventListener('resize', this.resizeListener);

    // Obtain current options based on matching media queries (if responsive options are given)
    // This will also register a listener that is re-creating the chart based on media changes
    this.optionsProvider = Chartist.optionsProvider(this.options, this.responsiveOptions, this.eventEmitter);
    // Register options change listener that will trigger a chart update
    this.eventEmitter.addEventHandler('optionsChanged', function() {
      this.update();
    }.bind(this));

    // Before the first chart creation we need to register us with all plugins that are configured
    // Initialize all relevant plugins with our chart object and the plugin options specified in the config
    if(this.options.plugins) {
      this.options.plugins.forEach(function(plugin) {
        if(plugin instanceof Array) {
          plugin[0](this, plugin[1]);
        } else {
          plugin(this);
        }
      }.bind(this));
    }

    // Event for data transformation that allows to manipulate the data before it gets rendered in the charts
    this.eventEmitter.emit('data', {
      type: 'initial',
      data: this.data
    });

    // Create the first chart
    this.createChart(this.optionsProvider.getCurrentOptions());

    // As chart is initialized from the event loop now we can reset our timeout reference
    // This is important if the chart gets initialized on the same element twice
    this.initializeTimeoutId = undefined;
  }

  /**
   * Constructor of chart base class.
   *
   * @param query
   * @param data
   * @param defaultOptions
   * @param options
   * @param responsiveOptions
   * @constructor
   */
  function Base(query, data, defaultOptions, options, responsiveOptions) {
    this.container = Chartist.querySelector(query);
    this.data = data || {};
    this.data.labels = this.data.labels || [];
    this.data.series = this.data.series || [];
    this.defaultOptions = defaultOptions;
    this.options = options;
    this.responsiveOptions = responsiveOptions;
    this.eventEmitter = Chartist.EventEmitter();
    this.supportsForeignObject = Chartist.Svg.isSupported('Extensibility');
    this.supportsAnimations = Chartist.Svg.isSupported('AnimationEventsAttribute');
    this.resizeListener = function resizeListener(){
      this.update();
    }.bind(this);

    if(this.container) {
      // If chartist was already initialized in this container we are detaching all event listeners first
      if(this.container.__chartist__) {
        this.container.__chartist__.detach();
      }

      this.container.__chartist__ = this;
    }

    // Using event loop for first draw to make it possible to register event listeners in the same call stack where
    // the chart was created.
    this.initializeTimeoutId = setTimeout(initialize.bind(this), 0);
  }

  // Creating the chart base class
  Chartist.Base = Chartist.Class.extend({
    constructor: Base,
    optionsProvider: undefined,
    container: undefined,
    svg: undefined,
    eventEmitter: undefined,
    createChart: function() {
      throw new Error('Base chart type can\'t be instantiated!');
    },
    update: update,
    detach: detach,
    on: on,
    off: off,
    version: Chartist.version,
    supportsForeignObject: false
  });

}(window, document, Chartist));
;/**
 * Chartist SVG module for simple SVG DOM abstraction
 *
 * @module Chartist.Svg
 */
/* global Chartist */
(function(window, document, Chartist) {
  'use strict';

  /**
   * Chartist.Svg creates a new SVG object wrapper with a starting element. You can use the wrapper to fluently create sub-elements and modify them.
   *
   * @memberof Chartist.Svg
   * @constructor
   * @param {String|Element} name The name of the SVG element to create or an SVG dom element which should be wrapped into Chartist.Svg
   * @param {Object} attributes An object with properties that will be added as attributes to the SVG element that is created. Attributes with undefined values will not be added.
   * @param {String} className This class or class list will be added to the SVG element
   * @param {Object} parent The parent SVG wrapper object where this newly created wrapper and it's element will be attached to as child
   * @param {Boolean} insertFirst If this param is set to true in conjunction with a parent element the newly created element will be added as first child element in the parent element
   */
  function Svg(name, attributes, className, parent, insertFirst) {
    // If Svg is getting called with an SVG element we just return the wrapper
    if(name instanceof Element) {
      this._node = name;
    } else {
      this._node = document.createElementNS(Chartist.namespaces.svg, name);

      // If this is an SVG element created then custom namespace
      if(name === 'svg') {
        this.attr({
          'xmlns:ct': Chartist.namespaces.ct
        });
      }
    }

    if(attributes) {
      this.attr(attributes);
    }

    if(className) {
      this.addClass(className);
    }

    if(parent) {
      if (insertFirst && parent._node.firstChild) {
        parent._node.insertBefore(this._node, parent._node.firstChild);
      } else {
        parent._node.appendChild(this._node);
      }
    }
  }

  /**
   * Set attributes on the current SVG element of the wrapper you're currently working on.
   *
   * @memberof Chartist.Svg
   * @param {Object|String} attributes An object with properties that will be added as attributes to the SVG element that is created. Attributes with undefined values will not be added. If this parameter is a String then the function is used as a getter and will return the attribute value.
   * @param {String} [ns] If specified, the attribute will be obtained using getAttributeNs. In order to write namepsaced attributes you can use the namespace:attribute notation within the attributes object.
   * @return {Object|String} The current wrapper object will be returned so it can be used for chaining or the attribute value if used as getter function.
   */
  function attr(attributes, ns) {
    if(typeof attributes === 'string') {
      if(ns) {
        return this._node.getAttributeNS(ns, attributes);
      } else {
        return this._node.getAttribute(attributes);
      }
    }

    Object.keys(attributes).forEach(function(key) {
      // If the attribute value is undefined we can skip this one
      if(attributes[key] === undefined) {
        return;
      }

      if (key.indexOf(':') !== -1) {
        var namespacedAttribute = key.split(':');
        this._node.setAttributeNS(Chartist.namespaces[namespacedAttribute[0]], key, attributes[key]);
      } else {
        this._node.setAttribute(key, attributes[key]);
      }
    }.bind(this));

    return this;
  }

  /**
   * Create a new SVG element whose wrapper object will be selected for further operations. This way you can also create nested groups easily.
   *
   * @memberof Chartist.Svg
   * @param {String} name The name of the SVG element that should be created as child element of the currently selected element wrapper
   * @param {Object} [attributes] An object with properties that will be added as attributes to the SVG element that is created. Attributes with undefined values will not be added.
   * @param {String} [className] This class or class list will be added to the SVG element
   * @param {Boolean} [insertFirst] If this param is set to true in conjunction with a parent element the newly created element will be added as first child element in the parent element
   * @return {Chartist.Svg} Returns a Chartist.Svg wrapper object that can be used to modify the containing SVG data
   */
  function elem(name, attributes, className, insertFirst) {
    return new Chartist.Svg(name, attributes, className, this, insertFirst);
  }

  /**
   * Returns the parent Chartist.SVG wrapper object
   *
   * @memberof Chartist.Svg
   * @return {Chartist.Svg} Returns a Chartist.Svg wrapper around the parent node of the current node. If the parent node is not existing or it's not an SVG node then this function will return null.
   */
  function parent() {
    return this._node.parentNode instanceof SVGElement ? new Chartist.Svg(this._node.parentNode) : null;
  }

  /**
   * This method returns a Chartist.Svg wrapper around the root SVG element of the current tree.
   *
   * @memberof Chartist.Svg
   * @return {Chartist.Svg} The root SVG element wrapped in a Chartist.Svg element
   */
  function root() {
    var node = this._node;
    while(node.nodeName !== 'svg') {
      node = node.parentNode;
    }
    return new Chartist.Svg(node);
  }

  /**
   * Find the first child SVG element of the current element that matches a CSS selector. The returned object is a Chartist.Svg wrapper.
   *
   * @memberof Chartist.Svg
   * @param {String} selector A CSS selector that is used to query for child SVG elements
   * @return {Chartist.Svg} The SVG wrapper for the element found or null if no element was found
   */
  function querySelector(selector) {
    var foundNode = this._node.querySelector(selector);
    return foundNode ? new Chartist.Svg(foundNode) : null;
  }

  /**
   * Find the all child SVG elements of the current element that match a CSS selector. The returned object is a Chartist.Svg.List wrapper.
   *
   * @memberof Chartist.Svg
   * @param {String} selector A CSS selector that is used to query for child SVG elements
   * @return {Chartist.Svg.List} The SVG wrapper list for the element found or null if no element was found
   */
  function querySelectorAll(selector) {
    var foundNodes = this._node.querySelectorAll(selector);
    return foundNodes.length ? new Chartist.Svg.List(foundNodes) : null;
  }

  /**
   * Returns the underlying SVG node for the current element.
   *
   * @memberof Chartist.Svg
   * @returns {Node}
   */
  function getNode() {
    return this._node;
  }

  /**
   * This method creates a foreignObject (see https://developer.mozilla.org/en-US/docs/Web/SVG/Element/foreignObject) that allows to embed HTML content into a SVG graphic. With the help of foreignObjects you can enable the usage of regular HTML elements inside of SVG where they are subject for SVG positioning and transformation but the Browser will use the HTML rendering capabilities for the containing DOM.
   *
   * @memberof Chartist.Svg
   * @param {Node|String} content The DOM Node, or HTML string that will be converted to a DOM Node, that is then placed into and wrapped by the foreignObject
   * @param {String} [attributes] An object with properties that will be added as attributes to the foreignObject element that is created. Attributes with undefined values will not be added.
   * @param {String} [className] This class or class list will be added to the SVG element
   * @param {Boolean} [insertFirst] Specifies if the foreignObject should be inserted as first child
   * @return {Chartist.Svg} New wrapper object that wraps the foreignObject element
   */
  function foreignObject(content, attributes, className, insertFirst) {
    // If content is string then we convert it to DOM
    // TODO: Handle case where content is not a string nor a DOM Node
    if(typeof content === 'string') {
      var container = document.createElement('div');
      container.innerHTML = content;
      content = container.firstChild;
    }

    // Adding namespace to content element
    content.setAttribute('xmlns', Chartist.namespaces.xmlns);

    // Creating the foreignObject without required extension attribute (as described here
    // http://www.w3.org/TR/SVG/extend.html#ForeignObjectElement)
    var fnObj = this.elem('foreignObject', attributes, className, insertFirst);

    // Add content to foreignObjectElement
    fnObj._node.appendChild(content);

    return fnObj;
  }

  /**
   * This method adds a new text element to the current Chartist.Svg wrapper.
   *
   * @memberof Chartist.Svg
   * @param {String} t The text that should be added to the text element that is created
   * @return {Chartist.Svg} The same wrapper object that was used to add the newly created element
   */
  function text(t) {
    this._node.appendChild(document.createTextNode(t));
    return this;
  }

  /**
   * This method will clear all child nodes of the current wrapper object.
   *
   * @memberof Chartist.Svg
   * @return {Chartist.Svg} The same wrapper object that got emptied
   */
  function empty() {
    while (this._node.firstChild) {
      this._node.removeChild(this._node.firstChild);
    }

    return this;
  }

  /**
   * This method will cause the current wrapper to remove itself from its parent wrapper. Use this method if you'd like to get rid of an element in a given DOM structure.
   *
   * @memberof Chartist.Svg
   * @return {Chartist.Svg} The parent wrapper object of the element that got removed
   */
  function remove() {
    this._node.parentNode.removeChild(this._node);
    return this.parent();
  }

  /**
   * This method will replace the element with a new element that can be created outside of the current DOM.
   *
   * @memberof Chartist.Svg
   * @param {Chartist.Svg} newElement The new Chartist.Svg object that will be used to replace the current wrapper object
   * @return {Chartist.Svg} The wrapper of the new element
   */
  function replace(newElement) {
    this._node.parentNode.replaceChild(newElement._node, this._node);
    return newElement;
  }

  /**
   * This method will append an element to the current element as a child.
   *
   * @memberof Chartist.Svg
   * @param {Chartist.Svg} element The Chartist.Svg element that should be added as a child
   * @param {Boolean} [insertFirst] Specifies if the element should be inserted as first child
   * @return {Chartist.Svg} The wrapper of the appended object
   */
  function append(element, insertFirst) {
    if(insertFirst && this._node.firstChild) {
      this._node.insertBefore(element._node, this._node.firstChild);
    } else {
      this._node.appendChild(element._node);
    }

    return this;
  }

  /**
   * Returns an array of class names that are attached to the current wrapper element. This method can not be chained further.
   *
   * @memberof Chartist.Svg
   * @return {Array} A list of classes or an empty array if there are no classes on the current element
   */
  function classes() {
    return this._node.getAttribute('class') ? this._node.getAttribute('class').trim().split(/\s+/) : [];
  }

  /**
   * Adds one or a space separated list of classes to the current element and ensures the classes are only existing once.
   *
   * @memberof Chartist.Svg
   * @param {String} names A white space separated list of class names
   * @return {Chartist.Svg} The wrapper of the current element
   */
  function addClass(names) {
    this._node.setAttribute('class',
      this.classes(this._node)
        .concat(names.trim().split(/\s+/))
        .filter(function(elem, pos, self) {
          return self.indexOf(elem) === pos;
        }).join(' ')
    );

    return this;
  }

  /**
   * Removes one or a space separated list of classes from the current element.
   *
   * @memberof Chartist.Svg
   * @param {String} names A white space separated list of class names
   * @return {Chartist.Svg} The wrapper of the current element
   */
  function removeClass(names) {
    var removedClasses = names.trim().split(/\s+/);

    this._node.setAttribute('class', this.classes(this._node).filter(function(name) {
      return removedClasses.indexOf(name) === -1;
    }).join(' '));

    return this;
  }

  /**
   * Removes all classes from the current element.
   *
   * @memberof Chartist.Svg
   * @return {Chartist.Svg} The wrapper of the current element
   */
  function removeAllClasses() {
    this._node.setAttribute('class', '');

    return this;
  }

  /**
   * Get element height using `getBoundingClientRect`
   *
   * @memberof Chartist.Svg
   * @return {Number} The elements height in pixels
   */
  function height() {
    return this._node.getBoundingClientRect().height;
  }

  /**
   * Get element width using `getBoundingClientRect`
   *
   * @memberof Chartist.Core
   * @return {Number} The elements width in pixels
   */
  function width() {
    return this._node.getBoundingClientRect().width;
  }

  /**
   * The animate function lets you animate the current element with SMIL animations. You can add animations for multiple attributes at the same time by using an animation definition object. This object should contain SMIL animation attributes. Please refer to http://www.w3.org/TR/SVG/animate.html for a detailed specification about the available animation attributes. Additionally an easing property can be passed in the animation definition object. This can be a string with a name of an easing function in `Chartist.Svg.Easing` or an array with four numbers specifying a cubic Bézier curve.
   * **An animations object could look like this:**
   * ```javascript
   * element.animate({
   *   opacity: {
   *     dur: 1000,
   *     from: 0,
   *     to: 1
   *   },
   *   x1: {
   *     dur: '1000ms',
   *     from: 100,
   *     to: 200,
   *     easing: 'easeOutQuart'
   *   },
   *   y1: {
   *     dur: '2s',
   *     from: 0,
   *     to: 100
   *   }
   * });
   * ```
   * **Automatic unit conversion**
   * For the `dur` and the `begin` animate attribute you can also omit a unit by passing a number. The number will automatically be converted to milli seconds.
   * **Guided mode**
   * The default behavior of SMIL animations with offset using the `begin` attribute is that the attribute will keep it's original value until the animation starts. Mostly this behavior is not desired as you'd like to have your element attributes already initialized with the animation `from` value even before the animation starts. Also if you don't specify `fill="freeze"` on an animate element or if you delete the animation after it's done (which is done in guided mode) the attribute will switch back to the initial value. This behavior is also not desired when performing simple one-time animations. For one-time animations you'd want to trigger animations immediately instead of relative to the document begin time. That's why in guided mode Chartist.Svg will also use the `begin` property to schedule a timeout and manually start the animation after the timeout. If you're using multiple SMIL definition objects for an attribute (in an array), guided mode will be disabled for this attribute, even if you explicitly enabled it.
   * If guided mode is enabled the following behavior is added:
   * - Before the animation starts (even when delayed with `begin`) the animated attribute will be set already to the `from` value of the animation
   * - `begin` is explicitly set to `indefinite` so it can be started manually without relying on document begin time (creation)
   * - The animate element will be forced to use `fill="freeze"`
   * - The animation will be triggered with `beginElement()` in a timeout where `begin` of the definition object is interpreted in milli seconds. If no `begin` was specified the timeout is triggered immediately.
   * - After the animation the element attribute value will be set to the `to` value of the animation
   * - The animate element is deleted from the DOM
   *
   * @memberof Chartist.Svg
   * @param {Object} animations An animations object where the property keys are the attributes you'd like to animate. The properties should be objects again that contain the SMIL animation attributes (usually begin, dur, from, and to). The property begin and dur is auto converted (see Automatic unit conversion). You can also schedule multiple animations for the same attribute by passing an Array of SMIL definition objects. Attributes that contain an array of SMIL definition objects will not be executed in guided mode.
   * @param {Boolean} guided Specify if guided mode should be activated for this animation (see Guided mode). If not otherwise specified, guided mode will be activated.
   * @param {Object} eventEmitter If specified, this event emitter will be notified when an animation starts or ends.
   * @return {Chartist.Svg} The current element where the animation was added
   */
  function animate(animations, guided, eventEmitter) {
    if(guided === undefined) {
      guided = true;
    }

    Object.keys(animations).forEach(function createAnimateForAttributes(attribute) {

      function createAnimate(animationDefinition, guided) {
        var attributeProperties = {},
          animate,
          timeout,
          easing;

        // Check if an easing is specified in the definition object and delete it from the object as it will not
        // be part of the animate element attributes.
        if(animationDefinition.easing) {
          // If already an easing Bézier curve array we take it or we lookup a easing array in the Easing object
          easing = animationDefinition.easing instanceof Array ?
            animationDefinition.easing :
            Chartist.Svg.Easing[animationDefinition.easing];
          delete animationDefinition.easing;
        }

        // If numeric dur or begin was provided we assume milli seconds
        animationDefinition.begin = Chartist.ensureUnit(animationDefinition.begin, 'ms');
        animationDefinition.dur = Chartist.ensureUnit(animationDefinition.dur, 'ms');

        if(easing) {
          animationDefinition.calcMode = 'spline';
          animationDefinition.keySplines = easing.join(' ');
          animationDefinition.keyTimes = '0;1';
        }

        // Adding "fill: freeze" if we are in guided mode and set initial attribute values
        if(guided) {
          animationDefinition.fill = 'freeze';
          // Animated property on our element should already be set to the animation from value in guided mode
          attributeProperties[attribute] = animationDefinition.from;
          this.attr(attributeProperties);

          // In guided mode we also set begin to indefinite so we can trigger the start manually and put the begin
          // which needs to be in ms aside
          timeout = Chartist.quantity(animationDefinition.begin || 0).value;
          animationDefinition.begin = 'indefinite';
        }

        animate = this.elem('animate', Chartist.extend({
          attributeName: attribute
        }, animationDefinition));

        if(guided) {
          // If guided we take the value that was put aside in timeout and trigger the animation manually with a timeout
          setTimeout(function() {
            // If beginElement fails we set the animated attribute to the end position and remove the animate element
            // This happens if the SMIL ElementTimeControl interface is not supported or any other problems occured in
            // the browser. (Currently FF 34 does not support animate elements in foreignObjects)
            try {
              animate._node.beginElement();
            } catch(err) {
              // Set animated attribute to current animated value
              attributeProperties[attribute] = animationDefinition.to;
              this.attr(attributeProperties);
              // Remove the animate element as it's no longer required
              animate.remove();
            }
          }.bind(this), timeout);
        }

        if(eventEmitter) {
          animate._node.addEventListener('beginEvent', function handleBeginEvent() {
            eventEmitter.emit('animationBegin', {
              element: this,
              animate: animate._node,
              params: animationDefinition
            });
          }.bind(this));
        }

        animate._node.addEventListener('endEvent', function handleEndEvent() {
          if(eventEmitter) {
            eventEmitter.emit('animationEnd', {
              element: this,
              animate: animate._node,
              params: animationDefinition
            });
          }

          if(guided) {
            // Set animated attribute to current animated value
            attributeProperties[attribute] = animationDefinition.to;
            this.attr(attributeProperties);
            // Remove the animate element as it's no longer required
            animate.remove();
          }
        }.bind(this));
      }

      // If current attribute is an array of definition objects we create an animate for each and disable guided mode
      if(animations[attribute] instanceof Array) {
        animations[attribute].forEach(function(animationDefinition) {
          createAnimate.bind(this)(animationDefinition, false);
        }.bind(this));
      } else {
        createAnimate.bind(this)(animations[attribute], guided);
      }

    }.bind(this));

    return this;
  }

  Chartist.Svg = Chartist.Class.extend({
    constructor: Svg,
    attr: attr,
    elem: elem,
    parent: parent,
    root: root,
    querySelector: querySelector,
    querySelectorAll: querySelectorAll,
    getNode: getNode,
    foreignObject: foreignObject,
    text: text,
    empty: empty,
    remove: remove,
    replace: replace,
    append: append,
    classes: classes,
    addClass: addClass,
    removeClass: removeClass,
    removeAllClasses: removeAllClasses,
    height: height,
    width: width,
    animate: animate
  });

  /**
   * This method checks for support of a given SVG feature like Extensibility, SVG-animation or the like. Check http://www.w3.org/TR/SVG11/feature for a detailed list.
   *
   * @memberof Chartist.Svg
   * @param {String} feature The SVG 1.1 feature that should be checked for support.
   * @return {Boolean} True of false if the feature is supported or not
   */
  Chartist.Svg.isSupported = function(feature) {
    return document.implementation.hasFeature('http://www.w3.org/TR/SVG11/feature#' + feature, '1.1');
  };

  /**
   * This Object contains some standard easing cubic bezier curves. Then can be used with their name in the `Chartist.Svg.animate`. You can also extend the list and use your own name in the `animate` function. Click the show code button to see the available bezier functions.
   *
   * @memberof Chartist.Svg
   */
  var easingCubicBeziers = {
    easeInSine: [0.47, 0, 0.745, 0.715],
    easeOutSine: [0.39, 0.575, 0.565, 1],
    easeInOutSine: [0.445, 0.05, 0.55, 0.95],
    easeInQuad: [0.55, 0.085, 0.68, 0.53],
    easeOutQuad: [0.25, 0.46, 0.45, 0.94],
    easeInOutQuad: [0.455, 0.03, 0.515, 0.955],
    easeInCubic: [0.55, 0.055, 0.675, 0.19],
    easeOutCubic: [0.215, 0.61, 0.355, 1],
    easeInOutCubic: [0.645, 0.045, 0.355, 1],
    easeInQuart: [0.895, 0.03, 0.685, 0.22],
    easeOutQuart: [0.165, 0.84, 0.44, 1],
    easeInOutQuart: [0.77, 0, 0.175, 1],
    easeInQuint: [0.755, 0.05, 0.855, 0.06],
    easeOutQuint: [0.23, 1, 0.32, 1],
    easeInOutQuint: [0.86, 0, 0.07, 1],
    easeInExpo: [0.95, 0.05, 0.795, 0.035],
    easeOutExpo: [0.19, 1, 0.22, 1],
    easeInOutExpo: [1, 0, 0, 1],
    easeInCirc: [0.6, 0.04, 0.98, 0.335],
    easeOutCirc: [0.075, 0.82, 0.165, 1],
    easeInOutCirc: [0.785, 0.135, 0.15, 0.86],
    easeInBack: [0.6, -0.28, 0.735, 0.045],
    easeOutBack: [0.175, 0.885, 0.32, 1.275],
    easeInOutBack: [0.68, -0.55, 0.265, 1.55]
  };

  Chartist.Svg.Easing = easingCubicBeziers;

  /**
   * This helper class is to wrap multiple `Chartist.Svg` elements into a list where you can call the `Chartist.Svg` functions on all elements in the list with one call. This is helpful when you'd like to perform calls with `Chartist.Svg` on multiple elements.
   * An instance of this class is also returned by `Chartist.Svg.querySelectorAll`.
   *
   * @memberof Chartist.Svg
   * @param {Array<Node>|NodeList} nodeList An Array of SVG DOM nodes or a SVG DOM NodeList (as returned by document.querySelectorAll)
   * @constructor
   */
  function SvgList(nodeList) {
    var list = this;

    this.svgElements = [];
    for(var i = 0; i < nodeList.length; i++) {
      this.svgElements.push(new Chartist.Svg(nodeList[i]));
    }

    // Add delegation methods for Chartist.Svg
    Object.keys(Chartist.Svg.prototype).filter(function(prototypeProperty) {
      return ['constructor',
          'parent',
          'querySelector',
          'querySelectorAll',
          'replace',
          'append',
          'classes',
          'height',
          'width'].indexOf(prototypeProperty) === -1;
    }).forEach(function(prototypeProperty) {
      list[prototypeProperty] = function() {
        var args = Array.prototype.slice.call(arguments, 0);
        list.svgElements.forEach(function(element) {
          Chartist.Svg.prototype[prototypeProperty].apply(element, args);
        });
        return list;
      };
    });
  }

  Chartist.Svg.List = Chartist.Class.extend({
    constructor: SvgList
  });
}(window, document, Chartist));
;/**
 * Chartist SVG path module for SVG path description creation and modification.
 *
 * @module Chartist.Svg.Path
 */
/* global Chartist */
(function(window, document, Chartist) {
  'use strict';

  /**
   * Contains the descriptors of supported element types in a SVG path. Currently only move, line and curve are supported.
   *
   * @memberof Chartist.Svg.Path
   * @type {Object}
   */
  var elementDescriptions = {
    m: ['x', 'y'],
    l: ['x', 'y'],
    c: ['x1', 'y1', 'x2', 'y2', 'x', 'y'],
    a: ['rx', 'ry', 'xAr', 'lAf', 'sf', 'x', 'y']
  };

  /**
   * Default options for newly created SVG path objects.
   *
   * @memberof Chartist.Svg.Path
   * @type {Object}
   */
  var defaultOptions = {
    // The accuracy in digit count after the decimal point. This will be used to round numbers in the SVG path. If this option is set to false then no rounding will be performed.
    accuracy: 3
  };

  function element(command, params, pathElements, pos, relative, data) {
    var pathElement = Chartist.extend({
      command: relative ? command.toLowerCase() : command.toUpperCase()
    }, params, data ? { data: data } : {} );

    pathElements.splice(pos, 0, pathElement);
  }

  function forEachParam(pathElements, cb) {
    pathElements.forEach(function(pathElement, pathElementIndex) {
      elementDescriptions[pathElement.command.toLowerCase()].forEach(function(paramName, paramIndex) {
        cb(pathElement, paramName, pathElementIndex, paramIndex, pathElements);
      });
    });
  }

  /**
   * Used to construct a new path object.
   *
   * @memberof Chartist.Svg.Path
   * @param {Boolean} close If set to true then this path will be closed when stringified (with a Z at the end)
   * @param {Object} options Options object that overrides the default objects. See default options for more details.
   * @constructor
   */
  function SvgPath(close, options) {
    this.pathElements = [];
    this.pos = 0;
    this.close = close;
    this.options = Chartist.extend({}, defaultOptions, options);
  }

  /**
   * Gets or sets the current position (cursor) inside of the path. You can move around the cursor freely but limited to 0 or the count of existing elements. All modifications with element functions will insert new elements at the position of this cursor.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} [pos] If a number is passed then the cursor is set to this position in the path element array.
   * @return {Chartist.Svg.Path|Number} If the position parameter was passed then the return value will be the path object for easy call chaining. If no position parameter was passed then the current position is returned.
   */
  function position(pos) {
    if(pos !== undefined) {
      this.pos = Math.max(0, Math.min(this.pathElements.length, pos));
      return this;
    } else {
      return this.pos;
    }
  }

  /**
   * Removes elements from the path starting at the current position.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} count Number of path elements that should be removed from the current position.
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function remove(count) {
    this.pathElements.splice(this.pos, count);
    return this;
  }

  /**
   * Use this function to add a new move SVG path element.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} x The x coordinate for the move element.
   * @param {Number} y The y coordinate for the move element.
   * @param {Boolean} [relative] If set to true the move element will be created with relative coordinates (lowercase letter)
   * @param {*} [data] Any data that should be stored with the element object that will be accessible in pathElement
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function move(x, y, relative, data) {
    element('M', {
      x: +x,
      y: +y
    }, this.pathElements, this.pos++, relative, data);
    return this;
  }

  /**
   * Use this function to add a new line SVG path element.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} x The x coordinate for the line element.
   * @param {Number} y The y coordinate for the line element.
   * @param {Boolean} [relative] If set to true the line element will be created with relative coordinates (lowercase letter)
   * @param {*} [data] Any data that should be stored with the element object that will be accessible in pathElement
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function line(x, y, relative, data) {
    element('L', {
      x: +x,
      y: +y
    }, this.pathElements, this.pos++, relative, data);
    return this;
  }

  /**
   * Use this function to add a new curve SVG path element.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} x1 The x coordinate for the first control point of the bezier curve.
   * @param {Number} y1 The y coordinate for the first control point of the bezier curve.
   * @param {Number} x2 The x coordinate for the second control point of the bezier curve.
   * @param {Number} y2 The y coordinate for the second control point of the bezier curve.
   * @param {Number} x The x coordinate for the target point of the curve element.
   * @param {Number} y The y coordinate for the target point of the curve element.
   * @param {Boolean} [relative] If set to true the curve element will be created with relative coordinates (lowercase letter)
   * @param {*} [data] Any data that should be stored with the element object that will be accessible in pathElement
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function curve(x1, y1, x2, y2, x, y, relative, data) {
    element('C', {
      x1: +x1,
      y1: +y1,
      x2: +x2,
      y2: +y2,
      x: +x,
      y: +y
    }, this.pathElements, this.pos++, relative, data);
    return this;
  }

  /**
   * Use this function to add a new non-bezier curve SVG path element.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} rx The radius to be used for the x-axis of the arc.
   * @param {Number} ry The radius to be used for the y-axis of the arc.
   * @param {Number} xAr Defines the orientation of the arc
   * @param {Number} lAf Large arc flag
   * @param {Number} sf Sweep flag
   * @param {Number} x The x coordinate for the target point of the curve element.
   * @param {Number} y The y coordinate for the target point of the curve element.
   * @param {Boolean} [relative] If set to true the curve element will be created with relative coordinates (lowercase letter)
   * @param {*} [data] Any data that should be stored with the element object that will be accessible in pathElement
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function arc(rx, ry, xAr, lAf, sf, x, y, relative, data) {
    element('A', {
      rx: +rx,
      ry: +ry,
      xAr: +xAr,
      lAf: +lAf,
      sf: +sf,
      x: +x,
      y: +y
    }, this.pathElements, this.pos++, relative, data);
    return this;
  }

  /**
   * Parses an SVG path seen in the d attribute of path elements, and inserts the parsed elements into the existing path object at the current cursor position. Any closing path indicators (Z at the end of the path) will be ignored by the parser as this is provided by the close option in the options of the path object.
   *
   * @memberof Chartist.Svg.Path
   * @param {String} path Any SVG path that contains move (m), line (l) or curve (c) components.
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function parse(path) {
    // Parsing the SVG path string into an array of arrays [['M', '10', '10'], ['L', '100', '100']]
    var chunks = path.replace(/([A-Za-z])([0-9])/g, '$1 $2')
      .replace(/([0-9])([A-Za-z])/g, '$1 $2')
      .split(/[\s,]+/)
      .reduce(function(result, element) {
        if(element.match(/[A-Za-z]/)) {
          result.push([]);
        }

        result[result.length - 1].push(element);
        return result;
      }, []);

    // If this is a closed path we remove the Z at the end because this is determined by the close option
    if(chunks[chunks.length - 1][0].toUpperCase() === 'Z') {
      chunks.pop();
    }

    // Using svgPathElementDescriptions to map raw path arrays into objects that contain the command and the parameters
    // For example {command: 'M', x: '10', y: '10'}
    var elements = chunks.map(function(chunk) {
        var command = chunk.shift(),
          description = elementDescriptions[command.toLowerCase()];

        return Chartist.extend({
          command: command
        }, description.reduce(function(result, paramName, index) {
          result[paramName] = +chunk[index];
          return result;
        }, {}));
      });

    // Preparing a splice call with the elements array as var arg params and insert the parsed elements at the current position
    var spliceArgs = [this.pos, 0];
    Array.prototype.push.apply(spliceArgs, elements);
    Array.prototype.splice.apply(this.pathElements, spliceArgs);
    // Increase the internal position by the element count
    this.pos += elements.length;

    return this;
  }

  /**
   * This function renders to current SVG path object into a final SVG string that can be used in the d attribute of SVG path elements. It uses the accuracy option to round big decimals. If the close parameter was set in the constructor of this path object then a path closing Z will be appended to the output string.
   *
   * @memberof Chartist.Svg.Path
   * @return {String}
   */
  function stringify() {
    var accuracyMultiplier = Math.pow(10, this.options.accuracy);

    return this.pathElements.reduce(function(path, pathElement) {
        var params = elementDescriptions[pathElement.command.toLowerCase()].map(function(paramName) {
          return this.options.accuracy ?
            (Math.round(pathElement[paramName] * accuracyMultiplier) / accuracyMultiplier) :
            pathElement[paramName];
        }.bind(this));

        return path + pathElement.command + params.join(',');
      }.bind(this), '') + (this.close ? 'Z' : '');
  }

  /**
   * Scales all elements in the current SVG path object. There is an individual parameter for each coordinate. Scaling will also be done for control points of curves, affecting the given coordinate.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} x The number which will be used to scale the x, x1 and x2 of all path elements.
   * @param {Number} y The number which will be used to scale the y, y1 and y2 of all path elements.
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function scale(x, y) {
    forEachParam(this.pathElements, function(pathElement, paramName) {
      pathElement[paramName] *= paramName[0] === 'x' ? x : y;
    });
    return this;
  }

  /**
   * Translates all elements in the current SVG path object. The translation is relative and there is an individual parameter for each coordinate. Translation will also be done for control points of curves, affecting the given coordinate.
   *
   * @memberof Chartist.Svg.Path
   * @param {Number} x The number which will be used to translate the x, x1 and x2 of all path elements.
   * @param {Number} y The number which will be used to translate the y, y1 and y2 of all path elements.
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function translate(x, y) {
    forEachParam(this.pathElements, function(pathElement, paramName) {
      pathElement[paramName] += paramName[0] === 'x' ? x : y;
    });
    return this;
  }

  /**
   * This function will run over all existing path elements and then loop over their attributes. The callback function will be called for every path element attribute that exists in the current path.
   * The method signature of the callback function looks like this:
   * ```javascript
   * function(pathElement, paramName, pathElementIndex, paramIndex, pathElements)
   * ```
   * If something else than undefined is returned by the callback function, this value will be used to replace the old value. This allows you to build custom transformations of path objects that can't be achieved using the basic transformation functions scale and translate.
   *
   * @memberof Chartist.Svg.Path
   * @param {Function} transformFnc The callback function for the transformation. Check the signature in the function description.
   * @return {Chartist.Svg.Path} The current path object for easy call chaining.
   */
  function transform(transformFnc) {
    forEachParam(this.pathElements, function(pathElement, paramName, pathElementIndex, paramIndex, pathElements) {
      var transformed = transformFnc(pathElement, paramName, pathElementIndex, paramIndex, pathElements);
      if(transformed || transformed === 0) {
        pathElement[paramName] = transformed;
      }
    });
    return this;
  }

  /**
   * This function clones a whole path object with all its properties. This is a deep clone and path element objects will also be cloned.
   *
   * @memberof Chartist.Svg.Path
   * @param {Boolean} [close] Optional option to set the new cloned path to closed. If not specified or false, the original path close option will be used.
   * @return {Chartist.Svg.Path}
   */
  function clone(close) {
    var c = new Chartist.Svg.Path(close || this.close);
    c.pos = this.pos;
    c.pathElements = this.pathElements.slice().map(function cloneElements(pathElement) {
      return Chartist.extend({}, pathElement);
    });
    c.options = Chartist.extend({}, this.options);
    return c;
  }

  /**
   * Split a Svg.Path object by a specific command in the path chain. The path chain will be split and an array of newly created paths objects will be returned. This is useful if you'd like to split an SVG path by it's move commands, for example, in order to isolate chunks of drawings.
   *
   * @memberof Chartist.Svg.Path
   * @param {String} command The command you'd like to use to split the path
   * @return {Array<Chartist.Svg.Path>}
   */
  function splitByCommand(command) {
    var split = [
      new Chartist.Svg.Path()
    ];

    this.pathElements.forEach(function(pathElement) {
      if(pathElement.command === command.toUpperCase() && split[split.length - 1].pathElements.length !== 0) {
        split.push(new Chartist.Svg.Path());
      }

      split[split.length - 1].pathElements.push(pathElement);
    });

    return split;
  }

  /**
   * This static function on `Chartist.Svg.Path` is joining multiple paths together into one paths.
   *
   * @memberof Chartist.Svg.Path
   * @param {Array<Chartist.Svg.Path>} paths A list of paths to be joined together. The order is important.
   * @param {boolean} close If the newly created path should be a closed path
   * @param {Object} options Path options for the newly created path.
   * @return {Chartist.Svg.Path}
   */

  function join(paths, close, options) {
    var joinedPath = new Chartist.Svg.Path(close, options);
    for(var i = 0; i < paths.length; i++) {
      var path = paths[i];
      for(var j = 0; j < path.pathElements.length; j++) {
        joinedPath.pathElements.push(path.pathElements[j]);
      }
    }
    return joinedPath;
  }

  Chartist.Svg.Path = Chartist.Class.extend({
    constructor: SvgPath,
    position: position,
    remove: remove,
    move: move,
    line: line,
    curve: curve,
    arc: arc,
    scale: scale,
    translate: translate,
    transform: transform,
    parse: parse,
    stringify: stringify,
    clone: clone,
    splitByCommand: splitByCommand
  });

  Chartist.Svg.Path.elementDescriptions = elementDescriptions;
  Chartist.Svg.Path.join = join;
}(window, document, Chartist));
;/* global Chartist */
(function (window, document, Chartist) {
  'use strict';

  var axisUnits = {
    x: {
      pos: 'x',
      len: 'width',
      dir: 'horizontal',
      rectStart: 'x1',
      rectEnd: 'x2',
      rectOffset: 'y2'
    },
    y: {
      pos: 'y',
      len: 'height',
      dir: 'vertical',
      rectStart: 'y2',
      rectEnd: 'y1',
      rectOffset: 'x1'
    }
  };

  function Axis(units, chartRect, ticks, options) {
    this.units = units;
    this.counterUnits = units === axisUnits.x ? axisUnits.y : axisUnits.x;
    this.chartRect = chartRect;
    this.axisLength = chartRect[units.rectEnd] - chartRect[units.rectStart];
    this.gridOffset = chartRect[units.rectOffset];
    this.ticks = ticks;
    this.options = options;
  }

  function createGridAndLabels(gridGroup, labelGroup, useForeignObject, chartOptions, eventEmitter) {
    var axisOptions = chartOptions['axis' + this.units.pos.toUpperCase()];
    var projectedValues = this.ticks.map(this.projectValue.bind(this));
    var labelValues = this.ticks.map(axisOptions.labelInterpolationFnc);

    projectedValues.forEach(function(projectedValue, index) {
      var labelOffset = {
        x: 0,
        y: 0
      };

      // TODO: Find better solution for solving this problem
      // Calculate how much space we have available for the label
      var labelLength;
      if(projectedValues[index + 1]) {
        // If we still have one label ahead, we can calculate the distance to the next tick / label
        labelLength = projectedValues[index + 1] - projectedValue;
      } else {
        // If we don't have a label ahead and we have only two labels in total, we just take the remaining distance to
        // on the whole axis length. We limit that to a minimum of 30 pixel, so that labels close to the border will
        // still be visible inside of the chart padding.
        labelLength = Math.max(this.axisLength - projectedValue, 30);
      }

      // Skip grid lines and labels where interpolated label values are falsey (execpt for 0)
      if(Chartist.isFalseyButZero(labelValues[index]) && labelValues[index] !== '') {
        return;
      }

      // Transform to global coordinates using the chartRect
      // We also need to set the label offset for the createLabel function
      if(this.units.pos === 'x') {
        projectedValue = this.chartRect.x1 + projectedValue;
        labelOffset.x = chartOptions.axisX.labelOffset.x;

        // If the labels should be positioned in start position (top side for vertical axis) we need to set a
        // different offset as for positioned with end (bottom)
        if(chartOptions.axisX.position === 'start') {
          labelOffset.y = this.chartRect.padding.top + chartOptions.axisX.labelOffset.y + (useForeignObject ? 5 : 20);
        } else {
          labelOffset.y = this.chartRect.y1 + chartOptions.axisX.labelOffset.y + (useForeignObject ? 5 : 20);
        }
      } else {
        projectedValue = this.chartRect.y1 - projectedValue;
        labelOffset.y = chartOptions.axisY.labelOffset.y - (useForeignObject ? labelLength : 0);

        // If the labels should be positioned in start position (left side for horizontal axis) we need to set a
        // different offset as for positioned with end (right side)
        if(chartOptions.axisY.position === 'start') {
          labelOffset.x = useForeignObject ? this.chartRect.padding.left + chartOptions.axisY.labelOffset.x : this.chartRect.x1 - 10;
        } else {
          labelOffset.x = this.chartRect.x2 + chartOptions.axisY.labelOffset.x + 10;
        }
      }

      if(axisOptions.showGrid) {
        Chartist.createGrid(projectedValue, index, this, this.gridOffset, this.chartRect[this.counterUnits.len](), gridGroup, [
          chartOptions.classNames.grid,
          chartOptions.classNames[this.units.dir]
        ], eventEmitter);
      }

      if(axisOptions.showLabel) {
        Chartist.createLabel(projectedValue, labelLength, index, labelValues, this, axisOptions.offset, labelOffset, labelGroup, [
          chartOptions.classNames.label,
          chartOptions.classNames[this.units.dir],
          (axisOptions.position === 'start' ? chartOptions.classNames[axisOptions.position] : chartOptions.classNames['end'])
        ], useForeignObject, eventEmitter);
      }
    }.bind(this));
  }

  Chartist.Axis = Chartist.Class.extend({
    constructor: Axis,
    createGridAndLabels: createGridAndLabels,
    projectValue: function(value, index, data) {
      throw new Error('Base axis can\'t be instantiated!');
    }
  });

  Chartist.Axis.units = axisUnits;

}(window, document, Chartist));
;/**
 * The auto scale axis uses standard linear scale projection of values along an axis. It uses order of magnitude to find a scale automatically and evaluates the available space in order to find the perfect amount of ticks for your chart.
 * **Options**
 * The following options are used by this axis in addition to the default axis options outlined in the axis configuration of the chart default settings.
 * ```javascript
 * var options = {
 *   // If high is specified then the axis will display values explicitly up to this value and the computed maximum from the data is ignored
 *   high: 100,
 *   // If low is specified then the axis will display values explicitly down to this value and the computed minimum from the data is ignored
 *   low: 0,
 *   // This option will be used when finding the right scale division settings. The amount of ticks on the scale will be determined so that as many ticks as possible will be displayed, while not violating this minimum required space (in pixel).
 *   scaleMinSpace: 20,
 *   // Can be set to true or false. If set to true, the scale will be generated with whole numbers only.
 *   onlyInteger: true,
 *   // The reference value can be used to make sure that this value will always be on the chart. This is especially useful on bipolar charts where the bipolar center always needs to be part of the chart.
 *   referenceValue: 5
 * };
 * ```
 *
 * @module Chartist.AutoScaleAxis
 */
/* global Chartist */
(function (window, document, Chartist) {
  'use strict';

  function AutoScaleAxis(axisUnit, data, chartRect, options) {
    // Usually we calculate highLow based on the data but this can be overriden by a highLow object in the options
    var highLow = options.highLow || Chartist.getHighLow(data, options, axisUnit.pos);
    this.bounds = Chartist.getBounds(chartRect[axisUnit.rectEnd] - chartRect[axisUnit.rectStart], highLow, options.scaleMinSpace || 20, options.onlyInteger);
    this.range = {
      min: this.bounds.min,
      max: this.bounds.max
    };

    Chartist.AutoScaleAxis.super.constructor.call(this,
      axisUnit,
      chartRect,
      this.bounds.values,
      options);
  }

  function projectValue(value) {
    return this.axisLength * (+Chartist.getMultiValue(value, this.units.pos) - this.bounds.min) / this.bounds.range;
  }

  Chartist.AutoScaleAxis = Chartist.Axis.extend({
    constructor: AutoScaleAxis,
    projectValue: projectValue
  });

}(window, document, Chartist));
;/**
 * The fixed scale axis uses standard linear projection of values along an axis. It makes use of a divisor option to divide the range provided from the minimum and maximum value or the options high and low that will override the computed minimum and maximum.
 * **Options**
 * The following options are used by this axis in addition to the default axis options outlined in the axis configuration of the chart default settings.
 * ```javascript
 * var options = {
 *   // If high is specified then the axis will display values explicitly up to this value and the computed maximum from the data is ignored
 *   high: 100,
 *   // If low is specified then the axis will display values explicitly down to this value and the computed minimum from the data is ignored
 *   low: 0,
 *   // If specified then the value range determined from minimum to maximum (or low and high) will be divided by this number and ticks will be generated at those division points. The default divisor is 1.
 *   divisor: 4,
 *   // If ticks is explicitly set, then the axis will not compute the ticks with the divisor, but directly use the data in ticks to determine at what points on the axis a tick need to be generated.
 *   ticks: [1, 10, 20, 30]
 * };
 * ```
 *
 * @module Chartist.FixedScaleAxis
 */
/* global Chartist */
(function (window, document, Chartist) {
  'use strict';

  function FixedScaleAxis(axisUnit, data, chartRect, options) {
    var highLow = options.highLow || Chartist.getHighLow(data, options, axisUnit.pos);
    this.divisor = options.divisor || 1;
    this.ticks = options.ticks || Chartist.times(this.divisor).map(function(value, index) {
      return highLow.low + (highLow.high - highLow.low) / this.divisor * index;
    }.bind(this));
    this.ticks.sort(function(a, b) {
      return a - b;
    });
    this.range = {
      min: highLow.low,
      max: highLow.high
    };

    Chartist.FixedScaleAxis.super.constructor.call(this,
      axisUnit,
      chartRect,
      this.ticks,
      options);

    this.stepLength = this.axisLength / this.divisor;
  }

  function projectValue(value) {
    return this.axisLength * (+Chartist.getMultiValue(value, this.units.pos) - this.range.min) / (this.range.max - this.range.min);
  }

  Chartist.FixedScaleAxis = Chartist.Axis.extend({
    constructor: FixedScaleAxis,
    projectValue: projectValue
  });

}(window, document, Chartist));
;/**
 * The step axis for step based charts like bar chart or step based line charts. It uses a fixed amount of ticks that will be equally distributed across the whole axis length. The projection is done using the index of the data value rather than the value itself and therefore it's only useful for distribution purpose.
 * **Options**
 * The following options are used by this axis in addition to the default axis options outlined in the axis configuration of the chart default settings.
 * ```javascript
 * var options = {
 *   // Ticks to be used to distribute across the axis length. As this axis type relies on the index of the value rather than the value, arbitrary data that can be converted to a string can be used as ticks.
 *   ticks: ['One', 'Two', 'Three'],
 *   // If set to true the full width will be used to distribute the values where the last value will be at the maximum of the axis length. If false the spaces between the ticks will be evenly distributed instead.
 *   stretch: true
 * };
 * ```
 *
 * @module Chartist.StepAxis
 */
/* global Chartist */
(function (window, document, Chartist) {
  'use strict';

  function StepAxis(axisUnit, data, chartRect, options) {
    Chartist.StepAxis.super.constructor.call(this,
      axisUnit,
      chartRect,
      options.ticks,
      options);

    var calc = Math.max(1, options.ticks.length - (options.stretch ? 1 : 0));
    this.stepLength = this.axisLength / calc;
  }

  function projectValue(value, index) {
    return this.stepLength * index;
  }

  Chartist.StepAxis = Chartist.Axis.extend({
    constructor: StepAxis,
    projectValue: projectValue
  });

}(window, document, Chartist));
;/**
 * The Chartist line chart can be used to draw Line or Scatter charts. If used in the browser you can access the global `Chartist` namespace where you find the `Line` function as a main entry point.
 *
 * For examples on how to use the line chart please check the examples of the `Chartist.Line` method.
 *
 * @module Chartist.Line
 */
/* global Chartist */
(function(window, document, Chartist){
  'use strict';

  /**
   * Default options in line charts. Expand the code view to see a detailed list of options with comments.
   *
   * @memberof Chartist.Line
   */
  var defaultOptions = {
    // Options for X-Axis
    axisX: {
      // The offset of the labels to the chart area
      offset: 30,
      // Position where labels are placed. Can be set to `start` or `end` where `start` is equivalent to left or top on vertical axis and `end` is equivalent to right or bottom on horizontal axis.
      position: 'end',
      // Allows you to correct label positioning on this axis by positive or negative x and y offset.
      labelOffset: {
        x: 0,
        y: 0
      },
      // If labels should be shown or not
      showLabel: true,
      // If the axis grid should be drawn or not
      showGrid: true,
      // Interpolation function that allows you to intercept the value from the axis label
      labelInterpolationFnc: Chartist.noop,
      // Set the axis type to be used to project values on this axis. If not defined, Chartist.StepAxis will be used for the X-Axis, where the ticks option will be set to the labels in the data and the stretch option will be set to the global fullWidth option. This type can be changed to any axis constructor available (e.g. Chartist.FixedScaleAxis), where all axis options should be present here.
      type: undefined
    },
    // Options for Y-Axis
    axisY: {
      // The offset of the labels to the chart area
      offset: 40,
      // Position where labels are placed. Can be set to `start` or `end` where `start` is equivalent to left or top on vertical axis and `end` is equivalent to right or bottom on horizontal axis.
      position: 'start',
      // Allows you to correct label positioning on this axis by positive or negative x and y offset.
      labelOffset: {
        x: 0,
        y: 0
      },
      // If labels should be shown or not
      showLabel: true,
      // If the axis grid should be drawn or not
      showGrid: true,
      // Interpolation function that allows you to intercept the value from the axis label
      labelInterpolationFnc: Chartist.noop,
      // Set the axis type to be used to project values on this axis. If not defined, Chartist.AutoScaleAxis will be used for the Y-Axis, where the high and low options will be set to the global high and low options. This type can be changed to any axis constructor available (e.g. Chartist.FixedScaleAxis), where all axis options should be present here.
      type: undefined,
      // This value specifies the minimum height in pixel of the scale steps
      scaleMinSpace: 20,
      // Use only integer values (whole numbers) for the scale steps
      onlyInteger: false
    },
    // Specify a fixed width for the chart as a string (i.e. '100px' or '50%')
    width: undefined,
    // Specify a fixed height for the chart as a string (i.e. '100px' or '50%')
    height: undefined,
    // If the line should be drawn or not
    showLine: true,
    // If dots should be drawn or not
    showPoint: true,
    // If the line chart should draw an area
    showArea: false,
    // The base for the area chart that will be used to close the area shape (is normally 0)
    areaBase: 0,
    // Specify if the lines should be smoothed. This value can be true or false where true will result in smoothing using the default smoothing interpolation function Chartist.Interpolation.cardinal and false results in Chartist.Interpolation.none. You can also choose other smoothing / interpolation functions available in the Chartist.Interpolation module, or write your own interpolation function. Check the examples for a brief description.
    lineSmooth: true,
    // If the line chart should add a background fill to the .ct-grids group.
    showGridBackground: false,
    // Overriding the natural low of the chart allows you to zoom in or limit the charts lowest displayed value
    low: undefined,
    // Overriding the natural high of the chart allows you to zoom in or limit the charts highest displayed value
    high: undefined,
    // Padding of the chart drawing area to the container element and labels as a number or padding object {top: 5, right: 5, bottom: 5, left: 5}
    chartPadding: {
      top: 15,
      right: 15,
      bottom: 5,
      left: 10
    },
    // When set to true, the last grid line on the x-axis is not drawn and the chart elements will expand to the full available width of the chart. For the last label to be drawn correctly you might need to add chart padding or offset the last label with a draw event handler.
    fullWidth: false,
    // If true the whole data is reversed including labels, the series order as well as the whole series data arrays.
    reverseData: false,
    // Override the class names that get used to generate the SVG structure of the chart
    classNames: {
      chart: 'ct-chart-line',
      label: 'ct-label',
      labelGroup: 'ct-labels',
      series: 'ct-series',
      line: 'ct-line',
      point: 'ct-point',
      area: 'ct-area',
      grid: 'ct-grid',
      gridGroup: 'ct-grids',
      gridBackground: 'ct-grid-background',
      vertical: 'ct-vertical',
      horizontal: 'ct-horizontal',
      start: 'ct-start',
      end: 'ct-end'
    }
  };

  /**
   * Creates a new chart
   *
   */
  function createChart(options) {
    var data = Chartist.normalizeData(this.data, options.reverseData, true);

    // Create new svg object
    this.svg = Chartist.createSvg(this.container, options.width, options.height, options.classNames.chart);
    // Create groups for labels, grid and series
    var gridGroup = this.svg.elem('g').addClass(options.classNames.gridGroup);
    var seriesGroup = this.svg.elem('g');
    var labelGroup = this.svg.elem('g').addClass(options.classNames.labelGroup);

    var chartRect = Chartist.createChartRect(this.svg, options, defaultOptions.padding);
    var axisX, axisY;

    if(options.axisX.type === undefined) {
      axisX = new Chartist.StepAxis(Chartist.Axis.units.x, data.normalized.series, chartRect, Chartist.extend({}, options.axisX, {
        ticks: data.normalized.labels,
        stretch: options.fullWidth
      }));
    } else {
      axisX = options.axisX.type.call(Chartist, Chartist.Axis.units.x, data.normalized.series, chartRect, options.axisX);
    }

    if(options.axisY.type === undefined) {
      axisY = new Chartist.AutoScaleAxis(Chartist.Axis.units.y, data.normalized.series, chartRect, Chartist.extend({}, options.axisY, {
        high: Chartist.isNumeric(options.high) ? options.high : options.axisY.high,
        low: Chartist.isNumeric(options.low) ? options.low : options.axisY.low
      }));
    } else {
      axisY = options.axisY.type.call(Chartist, Chartist.Axis.units.y, data.normalized.series, chartRect, options.axisY);
    }

    axisX.createGridAndLabels(gridGroup, labelGroup, this.supportsForeignObject, options, this.eventEmitter);
    axisY.createGridAndLabels(gridGroup, labelGroup, this.supportsForeignObject, options, this.eventEmitter);

    if (options.showGridBackground) {
      Chartist.createGridBackground(gridGroup, chartRect, options.classNames.gridBackground, this.eventEmitter);
    }

    // Draw the series
    data.raw.series.forEach(function(series, seriesIndex) {
      var seriesElement = seriesGroup.elem('g');

      // Write attributes to series group element. If series name or meta is undefined the attributes will not be written
      seriesElement.attr({
        'ct:series-name': series.name,
        'ct:meta': Chartist.serialize(series.meta)
      });

      // Use series class from series data or if not set generate one
      seriesElement.addClass([
        options.classNames.series,
        (series.className || options.classNames.series + '-' + Chartist.alphaNumerate(seriesIndex))
      ].join(' '));

      var pathCoordinates = [],
        pathData = [];

      data.normalized.series[seriesIndex].forEach(function(value, valueIndex) {
        var p = {
          x: chartRect.x1 + axisX.projectValue(value, valueIndex, data.normalized.series[seriesIndex]),
          y: chartRect.y1 - axisY.projectValue(value, valueIndex, data.normalized.series[seriesIndex])
        };
        pathCoordinates.push(p.x, p.y);
        pathData.push({
          value: value,
          valueIndex: valueIndex,
          meta: Chartist.getMetaData(series, valueIndex)
        });
      }.bind(this));

      var seriesOptions = {
        lineSmooth: Chartist.getSeriesOption(series, options, 'lineSmooth'),
        showPoint: Chartist.getSeriesOption(series, options, 'showPoint'),
        showLine: Chartist.getSeriesOption(series, options, 'showLine'),
        showArea: Chartist.getSeriesOption(series, options, 'showArea'),
        areaBase: Chartist.getSeriesOption(series, options, 'areaBase')
      };

      var smoothing = typeof seriesOptions.lineSmooth === 'function' ?
        seriesOptions.lineSmooth : (seriesOptions.lineSmooth ? Chartist.Interpolation.monotoneCubic() : Chartist.Interpolation.none());
      // Interpolating path where pathData will be used to annotate each path element so we can trace back the original
      // index, value and meta data
      var path = smoothing(pathCoordinates, pathData);

      // If we should show points we need to create them now to avoid secondary loop
      // Points are drawn from the pathElements returned by the interpolation function
      // Small offset for Firefox to render squares correctly
      if (seriesOptions.showPoint) {

        path.pathElements.forEach(function(pathElement) {
          var point = seriesElement.elem('line', {
            x1: pathElement.x,
            y1: pathElement.y,
            x2: pathElement.x + 0.01,
            y2: pathElement.y
          }, options.classNames.point).attr({
            'ct:value': [pathElement.data.value.x, pathElement.data.value.y].filter(Chartist.isNumeric).join(','),
            'ct:meta': Chartist.serialize(pathElement.data.meta)
          });

          this.eventEmitter.emit('draw', {
            type: 'point',
            value: pathElement.data.value,
            index: pathElement.data.valueIndex,
            meta: pathElement.data.meta,
            series: series,
            seriesIndex: seriesIndex,
            axisX: axisX,
            axisY: axisY,
            group: seriesElement,
            element: point,
            x: pathElement.x,
            y: pathElement.y
          });
        }.bind(this));
      }

      if(seriesOptions.showLine) {
        var line = seriesElement.elem('path', {
          d: path.stringify()
        }, options.classNames.line, true);

        this.eventEmitter.emit('draw', {
          type: 'line',
          values: data.normalized.series[seriesIndex],
          path: path.clone(),
          chartRect: chartRect,
          index: seriesIndex,
          series: series,
          seriesIndex: seriesIndex,
          seriesMeta: series.meta,
          axisX: axisX,
          axisY: axisY,
          group: seriesElement,
          element: line
        });
      }

      // Area currently only works with axes that support a range!
      if(seriesOptions.showArea && axisY.range) {
        // If areaBase is outside the chart area (< min or > max) we need to set it respectively so that
        // the area is not drawn outside the chart area.
        var areaBase = Math.max(Math.min(seriesOptions.areaBase, axisY.range.max), axisY.range.min);

        // We project the areaBase value into screen coordinates
        var areaBaseProjected = chartRect.y1 - axisY.projectValue(areaBase);

        // In order to form the area we'll first split the path by move commands so we can chunk it up into segments
        path.splitByCommand('M').filter(function onlySolidSegments(pathSegment) {
          // We filter only "solid" segments that contain more than one point. Otherwise there's no need for an area
          return pathSegment.pathElements.length > 1;
        }).map(function convertToArea(solidPathSegments) {
          // Receiving the filtered solid path segments we can now convert those segments into fill areas
          var firstElement = solidPathSegments.pathElements[0];
          var lastElement = solidPathSegments.pathElements[solidPathSegments.pathElements.length - 1];

          // Cloning the solid path segment with closing option and removing the first move command from the clone
          // We then insert a new move that should start at the area base and draw a straight line up or down
          // at the end of the path we add an additional straight line to the projected area base value
          // As the closing option is set our path will be automatically closed
          return solidPathSegments.clone(true)
            .position(0)
            .remove(1)
            .move(firstElement.x, areaBaseProjected)
            .line(firstElement.x, firstElement.y)
            .position(solidPathSegments.pathElements.length + 1)
            .line(lastElement.x, areaBaseProjected);

        }).forEach(function createArea(areaPath) {
          // For each of our newly created area paths, we'll now create path elements by stringifying our path objects
          // and adding the created DOM elements to the correct series group
          var area = seriesElement.elem('path', {
            d: areaPath.stringify()
          }, options.classNames.area, true);

          // Emit an event for each area that was drawn
          this.eventEmitter.emit('draw', {
            type: 'area',
            values: data.normalized.series[seriesIndex],
            path: areaPath.clone(),
            series: series,
            seriesIndex: seriesIndex,
            axisX: axisX,
            axisY: axisY,
            chartRect: chartRect,
            index: seriesIndex,
            group: seriesElement,
            element: area
          });
        }.bind(this));
      }
    }.bind(this));

    this.eventEmitter.emit('created', {
      bounds: axisY.bounds,
      chartRect: chartRect,
      axisX: axisX,
      axisY: axisY,
      svg: this.svg,
      options: options
    });
  }

  /**
   * This method creates a new line chart.
   *
   * @memberof Chartist.Line
   * @param {String|Node} query A selector query string or directly a DOM element
   * @param {Object} data The data object that needs to consist of a labels and a series array
   * @param {Object} [options] The options object with options that override the default options. Check the examples for a detailed list.
   * @param {Array} [responsiveOptions] Specify an array of responsive option arrays which are a media query and options object pair => [[mediaQueryString, optionsObject],[more...]]
   * @return {Object} An object which exposes the API for the created chart
   *
   * @example
   * // Create a simple line chart
   * var data = {
   *   // A labels array that can contain any sort of values
   *   labels: ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'],
   *   // Our series array that contains series objects or in this case series data arrays
   *   series: [
   *     [5, 2, 4, 2, 0]
   *   ]
   * };
   *
   * // As options we currently only set a static size of 300x200 px
   * var options = {
   *   width: '300px',
   *   height: '200px'
   * };
   *
   * // In the global name space Chartist we call the Line function to initialize a line chart. As a first parameter we pass in a selector where we would like to get our chart created. Second parameter is the actual data object and as a third parameter we pass in our options
   * new Chartist.Line('.ct-chart', data, options);
   *
   * @example
   * // Use specific interpolation function with configuration from the Chartist.Interpolation module
   *
   * var chart = new Chartist.Line('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5],
   *   series: [
   *     [1, 1, 8, 1, 7]
   *   ]
   * }, {
   *   lineSmooth: Chartist.Interpolation.cardinal({
   *     tension: 0.2
   *   })
   * });
   *
   * @example
   * // Create a line chart with responsive options
   *
   * var data = {
   *   // A labels array that can contain any sort of values
   *   labels: ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'],
   *   // Our series array that contains series objects or in this case series data arrays
   *   series: [
   *     [5, 2, 4, 2, 0]
   *   ]
   * };
   *
   * // In addition to the regular options we specify responsive option overrides that will override the default configutation based on the matching media queries.
   * var responsiveOptions = [
   *   ['screen and (min-width: 641px) and (max-width: 1024px)', {
   *     showPoint: false,
   *     axisX: {
   *       labelInterpolationFnc: function(value) {
   *         // Will return Mon, Tue, Wed etc. on medium screens
   *         return value.slice(0, 3);
   *       }
   *     }
   *   }],
   *   ['screen and (max-width: 640px)', {
   *     showLine: false,
   *     axisX: {
   *       labelInterpolationFnc: function(value) {
   *         // Will return M, T, W etc. on small screens
   *         return value[0];
   *       }
   *     }
   *   }]
   * ];
   *
   * new Chartist.Line('.ct-chart', data, null, responsiveOptions);
   *
   */
  function Line(query, data, options, responsiveOptions) {
    Chartist.Line.super.constructor.call(this,
      query,
      data,
      defaultOptions,
      Chartist.extend({}, defaultOptions, options),
      responsiveOptions);
  }

  // Creating line chart type in Chartist namespace
  Chartist.Line = Chartist.Base.extend({
    constructor: Line,
    createChart: createChart
  });

}(window, document, Chartist));
;/**
 * The bar chart module of Chartist that can be used to draw unipolar or bipolar bar and grouped bar charts.
 *
 * @module Chartist.Bar
 */
/* global Chartist */
(function(window, document, Chartist){
  'use strict';

  /**
   * Default options in bar charts. Expand the code view to see a detailed list of options with comments.
   *
   * @memberof Chartist.Bar
   */
  var defaultOptions = {
    // Options for X-Axis
    axisX: {
      // The offset of the chart drawing area to the border of the container
      offset: 30,
      // Position where labels are placed. Can be set to `start` or `end` where `start` is equivalent to left or top on vertical axis and `end` is equivalent to right or bottom on horizontal axis.
      position: 'end',
      // Allows you to correct label positioning on this axis by positive or negative x and y offset.
      labelOffset: {
        x: 0,
        y: 0
      },
      // If labels should be shown or not
      showLabel: true,
      // If the axis grid should be drawn or not
      showGrid: true,
      // Interpolation function that allows you to intercept the value from the axis label
      labelInterpolationFnc: Chartist.noop,
      // This value specifies the minimum width in pixel of the scale steps
      scaleMinSpace: 30,
      // Use only integer values (whole numbers) for the scale steps
      onlyInteger: false
    },
    // Options for Y-Axis
    axisY: {
      // The offset of the chart drawing area to the border of the container
      offset: 40,
      // Position where labels are placed. Can be set to `start` or `end` where `start` is equivalent to left or top on vertical axis and `end` is equivalent to right or bottom on horizontal axis.
      position: 'start',
      // Allows you to correct label positioning on this axis by positive or negative x and y offset.
      labelOffset: {
        x: 0,
        y: 0
      },
      // If labels should be shown or not
      showLabel: true,
      // If the axis grid should be drawn or not
      showGrid: true,
      // Interpolation function that allows you to intercept the value from the axis label
      labelInterpolationFnc: Chartist.noop,
      // This value specifies the minimum height in pixel of the scale steps
      scaleMinSpace: 20,
      // Use only integer values (whole numbers) for the scale steps
      onlyInteger: false
    },
    // Specify a fixed width for the chart as a string (i.e. '100px' or '50%')
    width: undefined,
    // Specify a fixed height for the chart as a string (i.e. '100px' or '50%')
    height: undefined,
    // Overriding the natural high of the chart allows you to zoom in or limit the charts highest displayed value
    high: undefined,
    // Overriding the natural low of the chart allows you to zoom in or limit the charts lowest displayed value
    low: undefined,
    // Unless low/high are explicitly set, bar chart will be centered at zero by default. Set referenceValue to null to auto scale.
    referenceValue: 0,
    // Padding of the chart drawing area to the container element and labels as a number or padding object {top: 5, right: 5, bottom: 5, left: 5}
    chartPadding: {
      top: 15,
      right: 15,
      bottom: 5,
      left: 10
    },
    // Specify the distance in pixel of bars in a group
    seriesBarDistance: 15,
    // If set to true this property will cause the series bars to be stacked. Check the `stackMode` option for further stacking options.
    stackBars: false,
    // If set to 'overlap' this property will force the stacked bars to draw from the zero line.
    // If set to 'accumulate' this property will form a total for each series point. This will also influence the y-axis and the overall bounds of the chart. In stacked mode the seriesBarDistance property will have no effect.
    stackMode: 'accumulate',
    // Inverts the axes of the bar chart in order to draw a horizontal bar chart. Be aware that you also need to invert your axis settings as the Y Axis will now display the labels and the X Axis the values.
    horizontalBars: false,
    // If set to true then each bar will represent a series and the data array is expected to be a one dimensional array of data values rather than a series array of series. This is useful if the bar chart should represent a profile rather than some data over time.
    distributeSeries: false,
    // If true the whole data is reversed including labels, the series order as well as the whole series data arrays.
    reverseData: false,
    // If the bar chart should add a background fill to the .ct-grids group.
    showGridBackground: false,
    // Override the class names that get used to generate the SVG structure of the chart
    classNames: {
      chart: 'ct-chart-bar',
      horizontalBars: 'ct-horizontal-bars',
      label: 'ct-label',
      labelGroup: 'ct-labels',
      series: 'ct-series',
      bar: 'ct-bar',
      grid: 'ct-grid',
      gridGroup: 'ct-grids',
      gridBackground: 'ct-grid-background',
      vertical: 'ct-vertical',
      horizontal: 'ct-horizontal',
      start: 'ct-start',
      end: 'ct-end'
    }
  };

  /**
   * Creates a new chart
   *
   */
  function createChart(options) {
    var data;
    var highLow;

    if(options.distributeSeries) {
      data = Chartist.normalizeData(this.data, options.reverseData, options.horizontalBars ? 'x' : 'y');
      data.normalized.series = data.normalized.series.map(function(value) {
        return [value];
      });
    } else {
      data = Chartist.normalizeData(this.data, options.reverseData, options.horizontalBars ? 'x' : 'y');
    }

    // Create new svg element
    this.svg = Chartist.createSvg(
      this.container,
      options.width,
      options.height,
      options.classNames.chart + (options.horizontalBars ? ' ' + options.classNames.horizontalBars : '')
    );

    // Drawing groups in correct order
    var gridGroup = this.svg.elem('g').addClass(options.classNames.gridGroup);
    var seriesGroup = this.svg.elem('g');
    var labelGroup = this.svg.elem('g').addClass(options.classNames.labelGroup);

    if(options.stackBars && data.normalized.series.length !== 0) {

      // If stacked bars we need to calculate the high low from stacked values from each series
      var serialSums = Chartist.serialMap(data.normalized.series, function serialSums() {
        return Array.prototype.slice.call(arguments).map(function(value) {
          return value;
        }).reduce(function(prev, curr) {
          return {
            x: prev.x + (curr && curr.x) || 0,
            y: prev.y + (curr && curr.y) || 0
          };
        }, {x: 0, y: 0});
      });

      highLow = Chartist.getHighLow([serialSums], options, options.horizontalBars ? 'x' : 'y');

    } else {

      highLow = Chartist.getHighLow(data.normalized.series, options, options.horizontalBars ? 'x' : 'y');
    }

    // Overrides of high / low from settings
    highLow.high = +options.high || (options.high === 0 ? 0 : highLow.high);
    highLow.low = +options.low || (options.low === 0 ? 0 : highLow.low);

    var chartRect = Chartist.createChartRect(this.svg, options, defaultOptions.padding);

    var valueAxis,
      labelAxisTicks,
      labelAxis,
      axisX,
      axisY;

    // We need to set step count based on some options combinations
    if(options.distributeSeries && options.stackBars) {
      // If distributed series are enabled and bars need to be stacked, we'll only have one bar and therefore should
      // use only the first label for the step axis
      labelAxisTicks = data.normalized.labels.slice(0, 1);
    } else {
      // If distributed series are enabled but stacked bars aren't, we should use the series labels
      // If we are drawing a regular bar chart with two dimensional series data, we just use the labels array
      // as the bars are normalized
      labelAxisTicks = data.normalized.labels;
    }

    // Set labelAxis and valueAxis based on the horizontalBars setting. This setting will flip the axes if necessary.
    if(options.horizontalBars) {
      if(options.axisX.type === undefined) {
        valueAxis = axisX = new Chartist.AutoScaleAxis(Chartist.Axis.units.x, data.normalized.series, chartRect, Chartist.extend({}, options.axisX, {
          highLow: highLow,
          referenceValue: 0
        }));
      } else {
        valueAxis = axisX = options.axisX.type.call(Chartist, Chartist.Axis.units.x, data.normalized.series, chartRect, Chartist.extend({}, options.axisX, {
          highLow: highLow,
          referenceValue: 0
        }));
      }

      if(options.axisY.type === undefined) {
        labelAxis = axisY = new Chartist.StepAxis(Chartist.Axis.units.y, data.normalized.series, chartRect, {
          ticks: labelAxisTicks
        });
      } else {
        labelAxis = axisY = options.axisY.type.call(Chartist, Chartist.Axis.units.y, data.normalized.series, chartRect, options.axisY);
      }
    } else {
      if(options.axisX.type === undefined) {
        labelAxis = axisX = new Chartist.StepAxis(Chartist.Axis.units.x, data.normalized.series, chartRect, {
          ticks: labelAxisTicks
        });
      } else {
        labelAxis = axisX = options.axisX.type.call(Chartist, Chartist.Axis.units.x, data.normalized.series, chartRect, options.axisX);
      }

      if(options.axisY.type === undefined) {
        valueAxis = axisY = new Chartist.AutoScaleAxis(Chartist.Axis.units.y, data.normalized.series, chartRect, Chartist.extend({}, options.axisY, {
          highLow: highLow,
          referenceValue: 0
        }));
      } else {
        valueAxis = axisY = options.axisY.type.call(Chartist, Chartist.Axis.units.y, data.normalized.series, chartRect, Chartist.extend({}, options.axisY, {
          highLow: highLow,
          referenceValue: 0
        }));
      }
    }

    // Projected 0 point
    var zeroPoint = options.horizontalBars ? (chartRect.x1 + valueAxis.projectValue(0)) : (chartRect.y1 - valueAxis.projectValue(0));
    // Used to track the screen coordinates of stacked bars
    var stackedBarValues = [];

    labelAxis.createGridAndLabels(gridGroup, labelGroup, this.supportsForeignObject, options, this.eventEmitter);
    valueAxis.createGridAndLabels(gridGroup, labelGroup, this.supportsForeignObject, options, this.eventEmitter);

    if (options.showGridBackground) {
      Chartist.createGridBackground(gridGroup, chartRect, options.classNames.gridBackground, this.eventEmitter);
    }

    // Draw the series
    data.raw.series.forEach(function(series, seriesIndex) {
      // Calculating bi-polar value of index for seriesOffset. For i = 0..4 biPol will be -1.5, -0.5, 0.5, 1.5 etc.
      var biPol = seriesIndex - (data.raw.series.length - 1) / 2;
      // Half of the period width between vertical grid lines used to position bars
      var periodHalfLength;
      // Current series SVG element
      var seriesElement;

      // We need to set periodHalfLength based on some options combinations
      if(options.distributeSeries && !options.stackBars) {
        // If distributed series are enabled but stacked bars aren't, we need to use the length of the normaizedData array
        // which is the series count and divide by 2
        periodHalfLength = labelAxis.axisLength / data.normalized.series.length / 2;
      } else if(options.distributeSeries && options.stackBars) {
        // If distributed series and stacked bars are enabled we'll only get one bar so we should just divide the axis
        // length by 2
        periodHalfLength = labelAxis.axisLength / 2;
      } else {
        // On regular bar charts we should just use the series length
        periodHalfLength = labelAxis.axisLength / data.normalized.series[seriesIndex].length / 2;
      }

      // Adding the series group to the series element
      seriesElement = seriesGroup.elem('g');

      // Write attributes to series group element. If series name or meta is undefined the attributes will not be written
      seriesElement.attr({
        'ct:series-name': series.name,
        'ct:meta': Chartist.serialize(series.meta)
      });

      // Use series class from series data or if not set generate one
      seriesElement.addClass([
        options.classNames.series,
        (series.className || options.classNames.series + '-' + Chartist.alphaNumerate(seriesIndex))
      ].join(' '));

      data.normalized.series[seriesIndex].forEach(function(value, valueIndex) {
        var projected,
          bar,
          previousStack,
          labelAxisValueIndex;

        // We need to set labelAxisValueIndex based on some options combinations
        if(options.distributeSeries && !options.stackBars) {
          // If distributed series are enabled but stacked bars aren't, we can use the seriesIndex for later projection
          // on the step axis for label positioning
          labelAxisValueIndex = seriesIndex;
        } else if(options.distributeSeries && options.stackBars) {
          // If distributed series and stacked bars are enabled, we will only get one bar and therefore always use
          // 0 for projection on the label step axis
          labelAxisValueIndex = 0;
        } else {
          // On regular bar charts we just use the value index to project on the label step axis
          labelAxisValueIndex = valueIndex;
        }

        // We need to transform coordinates differently based on the chart layout
        if(options.horizontalBars) {
          projected = {
            x: chartRect.x1 + valueAxis.projectValue(value && value.x ? value.x : 0, valueIndex, data.normalized.series[seriesIndex]),
            y: chartRect.y1 - labelAxis.projectValue(value && value.y ? value.y : 0, labelAxisValueIndex, data.normalized.series[seriesIndex])
          };
        } else {
          projected = {
            x: chartRect.x1 + labelAxis.projectValue(value && value.x ? value.x : 0, labelAxisValueIndex, data.normalized.series[seriesIndex]),
            y: chartRect.y1 - valueAxis.projectValue(value && value.y ? value.y : 0, valueIndex, data.normalized.series[seriesIndex])
          }
        }

        // If the label axis is a step based axis we will offset the bar into the middle of between two steps using
        // the periodHalfLength value. Also we do arrange the different series so that they align up to each other using
        // the seriesBarDistance. If we don't have a step axis, the bar positions can be chosen freely so we should not
        // add any automated positioning.
        if(labelAxis instanceof Chartist.StepAxis) {
          // Offset to center bar between grid lines, but only if the step axis is not stretched
          if(!labelAxis.options.stretch) {
            projected[labelAxis.units.pos] += periodHalfLength * (options.horizontalBars ? -1 : 1);
          }
          // Using bi-polar offset for multiple series if no stacked bars or series distribution is used
          projected[labelAxis.units.pos] += (options.stackBars || options.distributeSeries) ? 0 : biPol * options.seriesBarDistance * (options.horizontalBars ? -1 : 1);
        }

        // Enter value in stacked bar values used to remember previous screen value for stacking up bars
        previousStack = stackedBarValues[valueIndex] || zeroPoint;
        stackedBarValues[valueIndex] = previousStack - (zeroPoint - projected[labelAxis.counterUnits.pos]);

        // Skip if value is undefined
        if(value === undefined) {
          return;
        }

        var positions = {};
        positions[labelAxis.units.pos + '1'] = projected[labelAxis.units.pos];
        positions[labelAxis.units.pos + '2'] = projected[labelAxis.units.pos];

        if(options.stackBars && (options.stackMode === 'accumulate' || !options.stackMode)) {
          // Stack mode: accumulate (default)
          // If bars are stacked we use the stackedBarValues reference and otherwise base all bars off the zero line
          // We want backwards compatibility, so the expected fallback without the 'stackMode' option
          // to be the original behaviour (accumulate)
          positions[labelAxis.counterUnits.pos + '1'] = previousStack;
          positions[labelAxis.counterUnits.pos + '2'] = stackedBarValues[valueIndex];
        } else {
          // Draw from the zero line normally
          // This is also the same code for Stack mode: overlap
          positions[labelAxis.counterUnits.pos + '1'] = zeroPoint;
          positions[labelAxis.counterUnits.pos + '2'] = projected[labelAxis.counterUnits.pos];
        }

        // Limit x and y so that they are within the chart rect
        positions.x1 = Math.min(Math.max(positions.x1, chartRect.x1), chartRect.x2);
        positions.x2 = Math.min(Math.max(positions.x2, chartRect.x1), chartRect.x2);
        positions.y1 = Math.min(Math.max(positions.y1, chartRect.y2), chartRect.y1);
        positions.y2 = Math.min(Math.max(positions.y2, chartRect.y2), chartRect.y1);

        var metaData = Chartist.getMetaData(series, valueIndex);

        // Create bar element
        bar = seriesElement.elem('line', positions, options.classNames.bar).attr({
          'ct:value': [value.x, value.y].filter(Chartist.isNumeric).join(','),
          'ct:meta': Chartist.serialize(metaData)
        });

        this.eventEmitter.emit('draw', Chartist.extend({
          type: 'bar',
          value: value,
          index: valueIndex,
          meta: metaData,
          series: series,
          seriesIndex: seriesIndex,
          axisX: axisX,
          axisY: axisY,
          chartRect: chartRect,
          group: seriesElement,
          element: bar
        }, positions));
      }.bind(this));
    }.bind(this));

    this.eventEmitter.emit('created', {
      bounds: valueAxis.bounds,
      chartRect: chartRect,
      axisX: axisX,
      axisY: axisY,
      svg: this.svg,
      options: options
    });
  }

  /**
   * This method creates a new bar chart and returns API object that you can use for later changes.
   *
   * @memberof Chartist.Bar
   * @param {String|Node} query A selector query string or directly a DOM element
   * @param {Object} data The data object that needs to consist of a labels and a series array
   * @param {Object} [options] The options object with options that override the default options. Check the examples for a detailed list.
   * @param {Array} [responsiveOptions] Specify an array of responsive option arrays which are a media query and options object pair => [[mediaQueryString, optionsObject],[more...]]
   * @return {Object} An object which exposes the API for the created chart
   *
   * @example
   * // Create a simple bar chart
   * var data = {
   *   labels: ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'],
   *   series: [
   *     [5, 2, 4, 2, 0]
   *   ]
   * };
   *
   * // In the global name space Chartist we call the Bar function to initialize a bar chart. As a first parameter we pass in a selector where we would like to get our chart created and as a second parameter we pass our data object.
   * new Chartist.Bar('.ct-chart', data);
   *
   * @example
   * // This example creates a bipolar grouped bar chart where the boundaries are limitted to -10 and 10
   * new Chartist.Bar('.ct-chart', {
   *   labels: [1, 2, 3, 4, 5, 6, 7],
   *   series: [
   *     [1, 3, 2, -5, -3, 1, -6],
   *     [-5, -2, -4, -1, 2, -3, 1]
   *   ]
   * }, {
   *   seriesBarDistance: 12,
   *   low: -10,
   *   high: 10
   * });
   *
   */
  function Bar(query, data, options, responsiveOptions) {
    Chartist.Bar.super.constructor.call(this,
      query,
      data,
      defaultOptions,
      Chartist.extend({}, defaultOptions, options),
      responsiveOptions);
  }

  // Creating bar chart type in Chartist namespace
  Chartist.Bar = Chartist.Base.extend({
    constructor: Bar,
    createChart: createChart
  });

}(window, document, Chartist));
;/**
 * The pie chart module of Chartist that can be used to draw pie, donut or gauge charts
 *
 * @module Chartist.Pie
 */
/* global Chartist */
(function(window, document, Chartist) {
  'use strict';

  /**
   * Default options in line charts. Expand the code view to see a detailed list of options with comments.
   *
   * @memberof Chartist.Pie
   */
  var defaultOptions = {
    // Specify a fixed width for the chart as a string (i.e. '100px' or '50%')
    width: undefined,
    // Specify a fixed height for the chart as a string (i.e. '100px' or '50%')
    height: undefined,
    // Padding of the chart drawing area to the container element and labels as a number or padding object {top: 5, right: 5, bottom: 5, left: 5}
    chartPadding: 5,
    // Override the class names that are used to generate the SVG structure of the chart
    classNames: {
      chartPie: 'ct-chart-pie',
      chartDonut: 'ct-chart-donut',
      series: 'ct-series',
      slicePie: 'ct-slice-pie',
      sliceDonut: 'ct-slice-donut',
      sliceDonutSolid: 'ct-slice-donut-solid',
      label: 'ct-label'
    },
    // The start angle of the pie chart in degrees where 0 points north. A higher value offsets the start angle clockwise.
    startAngle: 0,
    // An optional total you can specify. By specifying a total value, the sum of the values in the series must be this total in order to draw a full pie. You can use this parameter to draw only parts of a pie or gauge charts.
    total: undefined,
    // If specified the donut CSS classes will be used and strokes will be drawn instead of pie slices.
    donut: false,
    // If specified the donut segments will be drawn as shapes instead of strokes.
    donutSolid: false,
    // Specify the donut stroke width, currently done in javascript for convenience. May move to CSS styles in the future.
    // This option can be set as number or string to specify a relative width (i.e. 100 or '30%').
    donutWidth: 60,
    // If a label should be shown or not
    showLabel: true,
    // Label position offset from the standard position which is half distance of the radius. This value can be either positive or negative. Positive values will position the label away from the center.
    labelOffset: 0,
    // This option can be set to 'inside', 'outside' or 'center'. Positioned with 'inside' the labels will be placed on half the distance of the radius to the border of the Pie by respecting the 'labelOffset'. The 'outside' option will place the labels at the border of the pie and 'center' will place the labels in the absolute center point of the chart. The 'center' option only makes sense in conjunction with the 'labelOffset' option.
    labelPosition: 'inside',
    // An interpolation function for the label value
    labelInterpolationFnc: Chartist.noop,
    // Label direction can be 'neutral', 'explode' or 'implode'. The labels anchor will be positioned based on those settings as well as the fact if the labels are on the right or left side of the center of the chart. Usually explode is useful when labels are positioned far away from the center.
    labelDirection: 'neutral',
    // If true the whole data is reversed including labels, the series order as well as the whole series data arrays.
    reverseData: false,
    // If true empty values will be ignored to avoid drawing unncessary slices and labels
    ignoreEmptyValues: false
  };

  /**
   * Determines SVG anchor position based on direction and center parameter
   *
   * @param center
   * @param label
   * @param direction
   * @return {string}
   */
  function determineAnchorPosition(center, label, direction) {
    var toTheRight = label.x > center.x;

    if(toTheRight && direction === 'explode' ||
      !toTheRight && direction === 'implode') {
      return 'start';
    } else if(toTheRight && direction === 'implode' ||
      !toTheRight && direction === 'explode') {
      return 'end';
    } else {
      return 'middle';
    }
  }

  /**
   * Creates the pie chart
   *
   * @param options
   */
  function createChart(options) {
    var data = Chartist.normalizeData(this.data);
    var seriesGroups = [],
      labelsGroup,
      chartRect,
      radius,
      labelRadius,
      totalDataSum,
      startAngle = options.startAngle;

    // Create SVG.js draw
    this.svg = Chartist.createSvg(this.container, options.width, options.height,options.donut ? options.classNames.chartDonut : options.classNames.chartPie);
    // Calculate charting rect
    chartRect = Chartist.createChartRect(this.svg, options, defaultOptions.padding);
    // Get biggest circle radius possible within chartRect
    radius = Math.min(chartRect.width() / 2, chartRect.height() / 2);
    // Calculate total of all series to get reference value or use total reference from optional options
    totalDataSum = options.total || data.normalized.series.reduce(function(previousValue, currentValue) {
      return previousValue + currentValue;
    }, 0);

    var donutWidth = Chartist.quantity(options.donutWidth);
    if (donutWidth.unit === '%') {
      donutWidth.value *= radius / 100;
    }

    // If this is a donut chart we need to adjust our radius to enable strokes to be drawn inside
    // Unfortunately this is not possible with the current SVG Spec
    // See this proposal for more details: http://lists.w3.org/Archives/Public/www-svg/2003Oct/0000.html
    radius -= options.donut && !options.donutSolid ? donutWidth.value / 2  : 0;

    // If labelPosition is set to `outside` or a donut chart is drawn then the label position is at the radius,
    // if regular pie chart it's half of the radius
    if(options.labelPosition === 'outside' || options.donut && !options.donutSolid) {
      labelRadius = radius;
    } else if(options.labelPosition === 'center') {
      // If labelPosition is center we start with 0 and will later wait for the labelOffset
      labelRadius = 0;
    } else if(options.donutSolid) {
      labelRadius = radius - donutWidth.value / 2;
    } else {
      // Default option is 'inside' where we use half the radius so the label will be placed in the center of the pie
      // slice
      labelRadius = radius / 2;
    }
    // Add the offset to the labelRadius where a negative offset means closed to the center of the chart
    labelRadius += options.labelOffset;

    // Calculate end angle based on total sum and current data value and offset with padding
    var center = {
      x: chartRect.x1 + chartRect.width() / 2,
      y: chartRect.y2 + chartRect.height() / 2
    };

    // Check if there is only one non-zero value in the series array.
    var hasSingleValInSeries = data.raw.series.filter(function(val) {
      return val.hasOwnProperty('value') ? val.value !== 0 : val !== 0;
    }).length === 1;

    // Creating the series groups
    data.raw.series.forEach(function(series, index) {
      seriesGroups[index] = this.svg.elem('g', null, null);
    }.bind(this));
    //if we need to show labels we create the label group now
    if(options.showLabel) {
      labelsGroup = this.svg.elem('g', null, null);
    }

    // Draw the series
    // initialize series groups
    data.raw.series.forEach(function(series, index) {
      // If current value is zero and we are ignoring empty values then skip to next value
      if (data.normalized.series[index] === 0 && options.ignoreEmptyValues) return;

      // If the series is an object and contains a name or meta data we add a custom attribute
      seriesGroups[index].attr({
        'ct:series-name': series.name
      });

      // Use series class from series data or if not set generate one
      seriesGroups[index].addClass([
        options.classNames.series,
        (series.className || options.classNames.series + '-' + Chartist.alphaNumerate(index))
      ].join(' '));

      // If the whole dataset is 0 endAngle should be zero. Can't divide by 0.
      var endAngle = (totalDataSum > 0 ? startAngle + data.normalized.series[index] / totalDataSum * 360 : 0);

      // Use slight offset so there are no transparent hairline issues
      var overlappigStartAngle = Math.max(0, startAngle - (index === 0 || hasSingleValInSeries ? 0 : 0.2));

      // If we need to draw the arc for all 360 degrees we need to add a hack where we close the circle
      // with Z and use 359.99 degrees
      if(endAngle - overlappigStartAngle >= 359.99) {
        endAngle = overlappigStartAngle + 359.99;
      }

      var start = Chartist.polarToCartesian(center.x, center.y, radius, overlappigStartAngle),
        end = Chartist.polarToCartesian(center.x, center.y, radius, endAngle);

      var innerStart,
        innerEnd,
        donutSolidRadius;

      // Create a new path element for the pie chart. If this isn't a donut chart we should close the path for a correct stroke
      var path = new Chartist.Svg.Path(!options.donut || options.donutSolid)
        .move(end.x, end.y)
        .arc(radius, radius, 0, endAngle - startAngle > 180, 0, start.x, start.y);

      // If regular pie chart (no donut) we add a line to the center of the circle for completing the pie
      if(!options.donut) {
        path.line(center.x, center.y);
      } else if (options.donutSolid) {
        donutSolidRadius = radius - donutWidth.value;
        innerStart = Chartist.polarToCartesian(center.x, center.y, donutSolidRadius, startAngle - (index === 0 || hasSingleValInSeries ? 0 : 0.2));
        innerEnd = Chartist.polarToCartesian(center.x, center.y, donutSolidRadius, endAngle);
        path.line(innerStart.x, innerStart.y);
        path.arc(donutSolidRadius, donutSolidRadius, 0, endAngle - startAngle  > 180, 1, innerEnd.x, innerEnd.y);
      }

      // Create the SVG path
      // If this is a donut chart we add the donut class, otherwise just a regular slice
      var pathClassName = options.classNames.slicePie;
      if (options.donut) {
        pathClassName = options.classNames.sliceDonut;
        if (options.donutSolid) {
          pathClassName = options.classNames.sliceDonutSolid;
        }
      }
      var pathElement = seriesGroups[index].elem('path', {
        d: path.stringify()
      }, pathClassName);

      // Adding the pie series value to the path
      pathElement.attr({
        'ct:value': data.normalized.series[index],
        'ct:meta': Chartist.serialize(series.meta)
      });

      // If this is a donut, we add the stroke-width as style attribute
      if(options.donut && !options.donutSolid) {
        pathElement._node.style.strokeWidth = donutWidth.value + 'px';
      }

      // Fire off draw event
      this.eventEmitter.emit('draw', {
        type: 'slice',
        value: data.normalized.series[index],
        totalDataSum: totalDataSum,
        index: index,
        meta: series.meta,
        series: series,
        group: seriesGroups[index],
        element: pathElement,
        path: path.clone(),
        center: center,
        radius: radius,
        startAngle: startAngle,
        endAngle: endAngle
      });

      // If we need to show labels we need to add the label for this slice now
      if(options.showLabel) {
        var labelPosition;
        if(data.raw.series.length === 1) {
          // If we have only 1 series, we can position the label in the center of the pie
          labelPosition = {
            x: center.x,
            y: center.y
          };
        } else {
          // Position at the labelRadius distance from center and between start and end angle
          labelPosition = Chartist.polarToCartesian(
            center.x,
            center.y,
            labelRadius,
            startAngle + (endAngle - startAngle) / 2
          );
        }

        var rawValue;
        if(data.normalized.labels && !Chartist.isFalseyButZero(data.normalized.labels[index])) {
          rawValue = data.normalized.labels[index];
        } else {
          rawValue = data.normalized.series[index];
        }

        var interpolatedValue = options.labelInterpolationFnc(rawValue, index);

        if(interpolatedValue || interpolatedValue === 0) {
          var labelElement = labelsGroup.elem('text', {
            dx: labelPosition.x,
            dy: labelPosition.y,
            'text-anchor': determineAnchorPosition(center, labelPosition, options.labelDirection)
          }, options.classNames.label).text('' + interpolatedValue);

          // Fire off draw event
          this.eventEmitter.emit('draw', {
            type: 'label',
            index: index,
            group: labelsGroup,
            element: labelElement,
            text: '' + interpolatedValue,
            x: labelPosition.x,
            y: labelPosition.y
          });
        }
      }

      // Set next startAngle to current endAngle.
      // (except for last slice)
      startAngle = endAngle;
    }.bind(this));

    this.eventEmitter.emit('created', {
      chartRect: chartRect,
      svg: this.svg,
      options: options
    });
  }

  /**
   * This method creates a new pie chart and returns an object that can be used to redraw the chart.
   *
   * @memberof Chartist.Pie
   * @param {String|Node} query A selector query string or directly a DOM element
   * @param {Object} data The data object in the pie chart needs to have a series property with a one dimensional data array. The values will be normalized against each other and don't necessarily need to be in percentage. The series property can also be an array of value objects that contain a value property and a className property to override the CSS class name for the series group.
   * @param {Object} [options] The options object with options that override the default options. Check the examples for a detailed list.
   * @param {Array} [responsiveOptions] Specify an array of responsive option arrays which are a media query and options object pair => [[mediaQueryString, optionsObject],[more...]]
   * @return {Object} An object with a version and an update method to manually redraw the chart
   *
   * @example
   * // Simple pie chart example with four series
   * new Chartist.Pie('.ct-chart', {
   *   series: [10, 2, 4, 3]
   * });
   *
   * @example
   * // Drawing a donut chart
   * new Chartist.Pie('.ct-chart', {
   *   series: [10, 2, 4, 3]
   * }, {
   *   donut: true
   * });
   *
   * @example
   * // Using donut, startAngle and total to draw a gauge chart
   * new Chartist.Pie('.ct-chart', {
   *   series: [20, 10, 30, 40]
   * }, {
   *   donut: true,
   *   donutWidth: 20,
   *   startAngle: 270,
   *   total: 200
   * });
   *
   * @example
   * // Drawing a pie chart with padding and labels that are outside the pie
   * new Chartist.Pie('.ct-chart', {
   *   series: [20, 10, 30, 40]
   * }, {
   *   chartPadding: 30,
   *   labelOffset: 50,
   *   labelDirection: 'explode'
   * });
   *
   * @example
   * // Overriding the class names for individual series as well as a name and meta data.
   * // The name will be written as ct:series-name attribute and the meta data will be serialized and written
   * // to a ct:meta attribute.
   * new Chartist.Pie('.ct-chart', {
   *   series: [{
   *     value: 20,
   *     name: 'Series 1',
   *     className: 'my-custom-class-one',
   *     meta: 'Meta One'
   *   }, {
   *     value: 10,
   *     name: 'Series 2',
   *     className: 'my-custom-class-two',
   *     meta: 'Meta Two'
   *   }, {
   *     value: 70,
   *     name: 'Series 3',
   *     className: 'my-custom-class-three',
   *     meta: 'Meta Three'
   *   }]
   * });
   */
  function Pie(query, data, options, responsiveOptions) {
    Chartist.Pie.super.constructor.call(this,
      query,
      data,
      defaultOptions,
      Chartist.extend({}, defaultOptions, options),
      responsiveOptions);
  }

  // Creating pie chart type in Chartist namespace
  Chartist.Pie = Chartist.Base.extend({
    constructor: Pie,
    createChart: createChart,
    determineAnchorPosition: determineAnchorPosition
  });

}(window, document, Chartist));

return Chartist;

}));

(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(["chartist"], function (Chartist) {
      return (root.returnExportsGlobal = factory(Chartist));
    });
  } else if (typeof exports === 'object') {
    // Node. Does not work with strict CommonJS, but
    // only CommonJS-like enviroments that support module.exports,
    // like Node.
    module.exports = factory(require("chartist"));
  } else {
    root['Chartist.plugins.tooltip'] = factory(Chartist);
  }
}(this, function (Chartist) {

  /**
  * Chartist.js plugin to display a data label on top of the points in a line chart.
  *
  */
  /* global Chartist */
  (function (window, document, Chartist) {
    'use strict';

    var defaultOptions = {
      currency: undefined,
      currencyFormatCallback: undefined,
      tooltipOffset: {
        x: 0,
        y: -20
      },
      anchorToPoint: false,
      appendToBody: false,
      class: undefined,
      pointClass: 'ct-point'
    };

    Chartist.plugins = Chartist.plugins || {};
    Chartist.plugins.tooltip = function (options) {
      options = Chartist.extend({}, defaultOptions, options);

      return function tooltip(chart) {
        var tooltipSelector = options.pointClass;
        if (chart.constructor.name == Chartist.Bar.prototype.constructor.name) {
          tooltipSelector = 'ct-bar';
        } else if (chart.constructor.name ==  Chartist.Pie.prototype.constructor.name) {
          // Added support for donut graph
          if (chart.options.donut) {
            tooltipSelector = 'ct-slice-donut';
          } else {
            tooltipSelector = 'ct-slice-pie';
          }
        }

        var $chart = chart.container;
        var $toolTip = $chart.querySelector('.chartist-tooltip');
        if (!$toolTip) {
          $toolTip = document.createElement('div');
          $toolTip.className = (!options.class) ? 'chartist-tooltip' : 'chartist-tooltip ' + options.class;
          if (!options.appendToBody) {
            $chart.appendChild($toolTip);
          } else {
            document.body.appendChild($toolTip);
          }
        }
        var height = $toolTip.offsetHeight;
        var width = $toolTip.offsetWidth;

        hide($toolTip);

        function on(event, selector, callback) {
          $chart.addEventListener(event, function (e) {
            if (!selector || hasClass(e.target, selector))
            callback(e);
          });
        }

        on('mouseover', tooltipSelector, function (event) {
          var $point = event.target;
          var tooltipText = '';

          var isPieChart = (chart instanceof Chartist.Pie) ? $point : $point.parentNode;
          var seriesName = (isPieChart) ? $point.parentNode.getAttribute('ct:meta') || $point.parentNode.getAttribute('ct:series-name') : '';
          var meta = $point.getAttribute('ct:meta') || seriesName || '';
          var hasMeta = !!meta;
          var value = $point.getAttribute('ct:value');

          if (options.transformTooltipTextFnc && typeof options.transformTooltipTextFnc === 'function') {
            value = options.transformTooltipTextFnc(value);
          }

          if (options.tooltipFnc && typeof options.tooltipFnc === 'function') {
            tooltipText = options.tooltipFnc(meta, value);
          } else {
            if (options.metaIsHTML) {
              var txt = document.createElement('textarea');
              txt.innerHTML = meta;
              meta = txt.value;
            }

            meta = '<span class="chartist-tooltip-meta">' + meta + '</span>';

            if (hasMeta) {
              tooltipText += meta + '<br>';
            } else {
              // For Pie Charts also take the labels into account
              // Could add support for more charts here as well!
              if (chart instanceof Chartist.Pie) {
                var label = next($point, 'ct-label');
                if (label) {
                  tooltipText += text(label) + '<br>';
                }
              }
            }

            if (value) {
              if (options.currency) {
                if (options.currencyFormatCallback != undefined) {
                  value = options.currencyFormatCallback(value, options);
                } else {
                  value = options.currency + value.replace(/(\d)(?=(\d{3})+(?:\.\d+)?$)/g, '$1,');
                }
              }
              value = '<span class="chartist-tooltip-value">' + value + '</span>';
              tooltipText += value;
            }
          }

          if(tooltipText) {
            $toolTip.innerHTML = tooltipText;
            setPosition(event);
            show($toolTip);

            // Remember height and width to avoid wrong position in IE
            height = $toolTip.offsetHeight;
            width = $toolTip.offsetWidth;
          }
        });

        on('mouseout', tooltipSelector, function () {
          hide($toolTip);
        });

        on('mousemove', null, function (event) {
          if (false === options.anchorToPoint)
          setPosition(event);
        });

        function setPosition(event) {
          height = height || $toolTip.offsetHeight;
          width = width || $toolTip.offsetWidth;
          var offsetX = - width / 2 + options.tooltipOffset.x
          var offsetY = - height + options.tooltipOffset.y;
          var anchorX, anchorY;

          if (!options.appendToBody) {
            var box = $chart.getBoundingClientRect();
            var left = event.pageX - box.left - window.pageXOffset ;
            var top = event.pageY - box.top - window.pageYOffset ;

            if (true === options.anchorToPoint && event.target.x2 && event.target.y2) {
              anchorX = parseInt(event.target.x2.baseVal.value);
              anchorY = parseInt(event.target.y2.baseVal.value);
            }

            $toolTip.style.top = (anchorY || top) + offsetY + 'px';
            $toolTip.style.left = (anchorX || left) + offsetX + 'px';
          } else {
            $toolTip.style.top = event.pageY + offsetY + 'px';
            $toolTip.style.left = event.pageX + offsetX + 'px';
          }
        }
      }
    };

    function show(element) {
      if(!hasClass(element, 'tooltip-show')) {
        element.className = element.className + ' tooltip-show';
      }
    }

    function hide(element) {
      var regex = new RegExp('tooltip-show' + '\\s*', 'gi');
      element.className = element.className.replace(regex, '').trim();
    }

    function hasClass(element, className) {
      return (' ' + element.getAttribute('class') + ' ').indexOf(' ' + className + ' ') > -1;
    }

    function next(element, className) {
      do {
        element = element.nextSibling;
      } while (element && !hasClass(element, className));
      return element;
    }

    function text(element) {
      return element.innerText || element.textContent;
    }

  } (window, document, Chartist));

  return Chartist.plugins.tooltip;

}));
