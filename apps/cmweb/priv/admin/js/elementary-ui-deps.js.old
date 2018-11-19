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
