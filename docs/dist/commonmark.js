"use strict";(()=>{var xe=Object.create;var W=Object.defineProperty;var we=Object.getOwnPropertyDescriptor;var ge=Object.getOwnPropertyNames;var Le=Object.getPrototypeOf,Ee=Object.prototype.hasOwnProperty;var G=(e,n)=>()=>(n||e((n={exports:{}}).exports,n),n.exports);var ze=(e,n,o,t)=>{if(n&&typeof n=="object"||typeof n=="function")for(let a of ge(n))!Ee.call(e,a)&&a!==o&&W(e,a,{get:()=>n[a],enumerable:!(t=we(n,a))||t.enumerable});return e};var J=(e,n,o)=>(o=e!=null?xe(Le(e)):{},ze(n||!e||!e.__esModule?W(o,"default",{value:e,enumerable:!0}):o,e));var V=G(g=>{"use strict";Object.defineProperty(g,"__esModule",{value:!0});g.StyledElement=g.CustomElement=void 0;var y=class extends HTMLElement{constructor(){super(),this.root=this.attachShadow({mode:"open"}),this.connected=!1}connectedCallback(){this.connected||(this.connect(),this.connected=!0)}};g.CustomElement=y;var R=class extends y{constructor(n){super();let o=document.createElement("link");o.setAttribute("rel","stylesheet");let t=document.currentScript.src,a=t.substring(0,t.lastIndexOf("/"));o.setAttribute("href",`${a}/${n}.css`),this.root.appendChild(o),this.body=document.createElement("div"),this.root.appendChild(this.body)}};g.StyledElement=R});var be=G((mn,Qe)=>{Qe.exports=[{markdown:`	foo	baz		bim
`,html:"<pre><code>foo	baz		bim</code></pre>",example:1,start_line:355,end_line:360,section:"Tabs"},{markdown:`  	foo	baz		bim
`,html:"<pre><code>foo	baz		bim</code></pre>",example:2,start_line:362,end_line:367,section:"Tabs"},{markdown:`    a	a
    \u1F50	a
`,html:`<pre><code>a	a
\u1F50	a</code></pre>`,example:3,start_line:369,end_line:376,section:"Tabs"},{markdown:`  - foo

	bar
`,html:"<ul><li><p>foo</p><p>bar</p></li></ul>",example:4,start_line:382,end_line:393,section:"Tabs"},{markdown:`- foo

		bar
`,html:`<ul>
<li>
<p>foo</p>
<pre><code>  bar
</code></pre>
</li>
</ul>
`,example:5,start_line:395,end_line:407,section:"Tabs"},{markdown:`>		foo
`,html:`<blockquote>
<pre><code>  foo
</code></pre>
</blockquote>
`,example:6,start_line:418,end_line:425,section:"Tabs"},{markdown:`-		foo
`,html:`<ul>
<li>
<pre><code>  foo
</code></pre>
</li>
</ul>
`,example:7,start_line:427,end_line:436,section:"Tabs"},{markdown:`    foo
	bar
`,html:`<pre><code>foo
bar</code></pre>`,example:8,start_line:439,end_line:446,section:"Tabs"},{markdown:` - foo
   - bar
	 - baz
`,html:`<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>baz</li>
</ul>
</li>
</ul>
</li>
</ul>
`,example:9,start_line:448,end_line:464,section:"Tabs"},{markdown:`#	Foo
`,html:"<h1>Foo</h1>",example:10,start_line:466,end_line:470,section:"Tabs"},{markdown:`*	*	*	
`,html:"<hr>",example:11,start_line:472,end_line:476,section:"Tabs"},{markdown:`\\!\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\\`\\{\\|\\}\\~
`,html:"<p>!\"#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>",example:12,start_line:489,end_line:493,section:"Backslash escapes"},{markdown:`\\	\\A\\a\\ \\3\\\u03C6\\\xAB
`,html:"<p>\\	\\A\\a\\ \\3\\\u03C6\\\xAB</p>",example:13,start_line:499,end_line:503,section:"Backslash escapes"},{markdown:`\\*not emphasized*
\\<br/> not a tag
\\[not a link](/foo)
\\\`not code\`
1\\. not a list
\\* not a list
\\# not a heading
\\[foo]: /url "not a reference"
\\&ouml; not a character entity
`,html:`<p>*not emphasized*
&lt;br/&gt; not a tag
[not a link](/foo)
\`not code\`
1. not a list
* not a list
# not a heading
[foo]: /url "not a reference"
&amp;ouml; not a character entity</p>`,example:14,start_line:509,end_line:529,section:"Backslash escapes"},{markdown:`\\\\*emphasis*
`,html:"<p>\\<em>emphasis</em></p>",example:15,start_line:534,end_line:538,section:"Backslash escapes"},{markdown:`foo\\
bar
`,html:`<p>foo<br>
bar</p>`,example:16,start_line:543,end_line:549,section:"Backslash escapes"},{markdown:"`` \\[\\` ``\n",html:"<p><code>\\[\\`</code></p>",example:17,start_line:555,end_line:559,section:"Backslash escapes"},{markdown:`    \\[\\]
`,html:"<pre><code>\\[\\]</code></pre>",example:18,start_line:562,end_line:567,section:"Backslash escapes"},{markdown:`~~~
\\[\\]
~~~
`,html:"<pre><code>\\[\\]</code></pre>",example:19,start_line:570,end_line:577,section:"Backslash escapes"},{markdown:`<https://example.com?find=\\*>
`,html:'<p><a href="https://example.com?find=\\*">https://example.com?find=\\*</a></p>',example:20,start_line:580,end_line:584,section:"Backslash escapes"},{markdown:`<a href="/bar\\/)">
`,html:`<a href="/bar\\/)">
</a>`,example:21,start_line:587,end_line:591,section:"Backslash escapes"},{markdown:`[foo](/bar\\* "ti\\*tle")
`,html:'<p><a href="/bar*" title="ti*tle">foo</a></p>',example:22,start_line:597,end_line:601,section:"Backslash escapes"},{markdown:`[foo]

[foo]: /bar\\* "ti\\*tle"
`,html:'<p><a href="/bar*" title="ti*tle">foo</a></p>',example:23,start_line:604,end_line:610,section:"Backslash escapes"},{markdown:"``` foo\\+bar\nfoo\n```\n",html:'<pre><code class="language-foo+bar">foo</code></pre>',example:24,start_line:613,end_line:620,section:"Backslash escapes"},{markdown:`&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;
`,html:`<p>&nbsp; &amp; \xA9 \xC6 \u010E
\xBE \u210B \u2146
\u2232 \u2267\u0338</p>`,example:25,start_line:649,end_line:657,section:"Entity and numeric character references"},{markdown:`&#35; &#1234; &#992; &#0;
`,html:"<p># \u04D2 \u03E0 \uFFFD</p>",example:26,start_line:668,end_line:672,section:"Entity and numeric character references"},{markdown:`&#X22; &#XD06; &#xcab;
`,html:'<p>" \u0D06 \u0CAB</p>',example:27,start_line:681,end_line:685,section:"Entity and numeric character references"},{markdown:`&nbsp &x; &#; &#x;
&#87654321;
&#abcdef0;
&ThisIsNotDefined; &hi?;
`,html:`<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;
&amp;#87654321;
&amp;#abcdef0;
&amp;ThisIsNotDefined; &amp;hi?;</p>`,example:28,start_line:690,end_line:700,section:"Entity and numeric character references"},{markdown:`&copy
`,html:"<p>&amp;copy</p>",example:29,start_line:707,end_line:711,section:"Entity and numeric character references"},{markdown:`&MadeUpEntity;
`,html:"<p>&amp;MadeUpEntity;</p>",example:30,start_line:717,end_line:721,section:"Entity and numeric character references"},{markdown:`<a href="&ouml;&ouml;.html">
`,html:`<a href="\xF6\xF6.html">
</a>`,example:31,start_line:728,end_line:732,section:"Entity and numeric character references"},{markdown:`[foo](/f&ouml;&ouml; "f&ouml;&ouml;")
`,html:`<p><a href="/f%C3%B6%C3%B6" title="f\xF6\xF6">foo</a></p>
`,example:32,start_line:735,end_line:739,section:"Entity and numeric character references"},{markdown:`[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"
`,html:`<p><a href="/f%C3%B6%C3%B6" title="f\xF6\xF6">foo</a></p>
`,example:33,start_line:742,end_line:748,section:"Entity and numeric character references"},{markdown:"``` f&ouml;&ouml;\nfoo\n```\n",html:`<pre><code class="language-f\xF6\xF6">foo
</code></pre>
`,example:34,start_line:751,end_line:758,section:"Entity and numeric character references"},{markdown:"`f&ouml;&ouml;`\n",html:"<p><code>f&amp;ouml;&amp;ouml;</code></p>",example:35,start_line:764,end_line:768,section:"Entity and numeric character references"},{markdown:`    f&ouml;f&ouml;
`,html:"<pre><code>f&amp;ouml;f&amp;ouml;</code></pre>",example:36,start_line:771,end_line:776,section:"Entity and numeric character references"},{markdown:`&#42;foo&#42;
*foo*
`,html:`<p>*foo*
<em>foo</em></p>`,example:37,start_line:783,end_line:789,section:"Entity and numeric character references"},{markdown:`&#42; foo

* foo`,html:"<p>* foo</p><ul><li>foo</li></ul>",example:38,start_line:791,end_line:800,section:"Entity and numeric character references"},{markdown:`foo&#10;&#10;bar
`,html:`<p>foo

bar</p>`,example:39,start_line:802,end_line:808,section:"Entity and numeric character references"},{markdown:`&#9;foo
`,html:"<p>	foo</p>",example:40,start_line:810,end_line:814,section:"Entity and numeric character references"},{markdown:`[a](url &quot;tit&quot;)
`,html:'<p>[a](url "tit")</p>',example:41,start_line:817,end_line:821,section:"Entity and numeric character references"},{markdown:"- `one\n- two`",html:"<ul><li>`one</li><li>two`</li></ul>",example:42,start_line:840,end_line:848,section:"Precedence"},{markdown:`***
---
___
`,html:"<hr><hr><hr>",example:43,start_line:879,end_line:887,section:"Thematic breaks"},{markdown:`+++
`,html:"<p>+++</p>",example:44,start_line:892,end_line:896,section:"Thematic breaks"},{markdown:`===
`,html:"<p>===</p>",example:45,start_line:899,end_line:903,section:"Thematic breaks"},{markdown:`--
**
__
`,html:`<p>--
**
__</p>`,example:46,start_line:908,end_line:916,section:"Thematic breaks"},{markdown:` ***
  ***
   ***
`,html:"<hr><hr><hr>",example:47,start_line:921,end_line:929,section:"Thematic breaks"},{markdown:`    ***
`,html:"<pre><code>***</code></pre>",example:48,start_line:934,end_line:939,section:"Thematic breaks"},{markdown:`Foo
    ***
`,html:`<p>Foo
***</p>`,example:49,start_line:942,end_line:948,section:"Thematic breaks"},{markdown:`_____________________________________
`,html:"<hr>",example:50,start_line:953,end_line:957,section:"Thematic breaks"},{markdown:` - - -
`,html:"<hr>",example:51,start_line:962,end_line:966,section:"Thematic breaks"},{markdown:` **  * ** * ** * **
`,html:"<hr>",example:52,start_line:969,end_line:973,section:"Thematic breaks"},{markdown:`-     -      -      -
`,html:"<hr>",example:53,start_line:976,end_line:980,section:"Thematic breaks"},{markdown:`- - - -    
`,html:"<hr>",example:54,start_line:985,end_line:989,section:"Thematic breaks"},{markdown:`_ _ _ _ a

a------

---a---
`,html:"<p>_ _ _ _ a</p><p>a------</p><p>---a---</p>",example:55,start_line:994,end_line:1004,section:"Thematic breaks"},{markdown:` *-*
`,html:"<p><em>-</em></p>",example:56,start_line:1010,end_line:1014,section:"Thematic breaks"},{markdown:`- foo
***
- bar`,html:"<ul><li>foo</li></ul><hr><ul><li>bar</li></ul>",example:57,start_line:1019,end_line:1031,section:"Thematic breaks"},{markdown:`Foo
***
bar
`,html:"<p>Foo</p><hr><p>bar</p>",example:58,start_line:1036,end_line:1044,section:"Thematic breaks"},{markdown:`Foo
---
bar
`,html:"<h2>Foo</h2><p>bar</p>",example:59,start_line:1053,end_line:1060,section:"Thematic breaks"},{markdown:`* Foo
* * *
* Bar
`,html:`<ul>
<li>Foo</li>
</ul>
<hr />
<ul>
<li>Bar</li>
</ul>
`,example:60,start_line:1066,end_line:1078,section:"Thematic breaks"},{markdown:`- Foo
- * * *
`,html:"<ul><li>Foo</li><li><hr></li></ul>",example:61,start_line:1083,end_line:1093,section:"Thematic breaks"},{markdown:`# foo
## foo
### foo
#### foo
##### foo
###### foo
`,html:"<h1>foo</h1><h2>foo</h2><h3>foo</h3><h4>foo</h4><h5>foo</h5><h6>foo</h6>",example:62,start_line:1112,end_line:1126,section:"ATX headings"},{markdown:`####### foo
`,html:"<p>####### foo</p>",example:63,start_line:1131,end_line:1135,section:"ATX headings"},{markdown:`#5 bolt

#hashtag
`,html:"<p>#5 bolt</p><p>#hashtag</p>",example:64,start_line:1146,end_line:1153,section:"ATX headings"},{markdown:`\\## foo
`,html:"<p>## foo</p>",example:65,start_line:1158,end_line:1162,section:"ATX headings"},{markdown:`# foo *bar* \\*baz\\*
`,html:"<h1>foo <em>bar</em> *baz*</h1>",example:66,start_line:1167,end_line:1171,section:"ATX headings"},{markdown:`#                  foo                     
`,html:"<h1>foo</h1>",example:67,start_line:1176,end_line:1180,section:"ATX headings"},{markdown:` ### foo
  ## foo
   # foo
`,html:"<h3>foo</h3><h2>foo</h2><h1>foo</h1>",example:68,start_line:1185,end_line:1193,section:"ATX headings"},{markdown:`    # foo
`,html:"<pre><code># foo</code></pre>",example:69,start_line:1198,end_line:1203,section:"ATX headings"},{markdown:`foo
    # bar
`,html:`<p>foo
# bar</p>`,example:70,start_line:1206,end_line:1212,section:"ATX headings"},{markdown:`## foo ##
  ###   bar    ###
`,html:`<h2>foo</h2>
<h3>bar</h3>
`,example:71,start_line:1217,end_line:1223,section:"ATX headings"},{markdown:`# foo ##################################
##### foo ##
`,html:`<h1>foo</h1>
<h5>foo</h5>
`,example:72,start_line:1228,end_line:1234,section:"ATX headings"},{markdown:`### foo ###     
`,html:`<h3>foo</h3>
`,example:73,start_line:1239,end_line:1243,section:"ATX headings"},{markdown:`### foo ### b
`,html:`<h3>foo ### b</h3>
`,example:74,start_line:1250,end_line:1254,section:"ATX headings"},{markdown:`# foo#
`,html:`<h1>foo#</h1>
`,example:75,start_line:1259,end_line:1263,section:"ATX headings"},{markdown:`### foo \\###
## foo #\\##
# foo \\#
`,html:`<h3>foo ###</h3>
<h2>foo ###</h2>
<h1>foo #</h1>
`,example:76,start_line:1269,end_line:1277,section:"ATX headings"},{markdown:`****
## foo
****
`,html:"<hr><h2>foo</h2><hr>",example:77,start_line:1283,end_line:1291,section:"ATX headings"},{markdown:`Foo bar
# baz
Bar foo
`,html:"<p>Foo bar</p><h1>baz</h1><p>Bar foo</p>",example:78,start_line:1294,end_line:1302,section:"ATX headings"},{markdown:`## 
# 
### 
`,html:"<h2></h2><h1></h1><h3></h3>",example:79,start_line:1307,end_line:1315,section:"ATX headings"},{markdown:`Foo *bar*
=========

Foo *bar*
---------
`,html:"<h1>Foo <em>bar</em></h1><h2>Foo <em>bar</em></h2>",example:80,start_line:1347,end_line:1356,section:"Setext headings"},{markdown:`Foo *bar
baz*
====
`,html:`<h1>Foo <em>bar
baz</em></h1>`,example:81,start_line:1361,end_line:1368,section:"Setext headings"},{markdown:`  Foo *bar
baz*	
====
`,html:`<h1>Foo <em>bar
baz</em></h1>`,example:82,start_line:1375,end_line:1382,section:"Setext headings"},{markdown:`Foo
-------------------------

Foo
=
`,html:"<h2>Foo</h2><h1>Foo</h1>",example:83,start_line:1387,end_line:1396,section:"Setext headings"},{markdown:`   Foo
---

  Foo
-----

  Foo
  ===
`,html:"<h2>Foo</h2><h2>Foo</h2><h1>Foo</h1>",example:84,start_line:1402,end_line:1415,section:"Setext headings"},{markdown:`    Foo
    ---

    Foo
---
`,html:`<pre><code>Foo
---

Foo</code></pre><hr>`,example:85,start_line:1420,end_line:1433,section:"Setext headings"},{markdown:`Foo
   ----      
`,html:"<h2>Foo</h2>",example:86,start_line:1439,end_line:1444,section:"Setext headings"},{markdown:`Foo
    ---
`,html:`<p>Foo
---</p>`,example:87,start_line:1449,end_line:1455,section:"Setext headings"},{markdown:`Foo
= =

Foo
--- -
`,html:`<p>Foo
= =</p><p>Foo</p><hr>`,example:88,start_line:1460,end_line:1471,section:"Setext headings"},{markdown:`Foo  
-----
`,html:"<h2>Foo</h2>",example:89,start_line:1476,end_line:1481,section:"Setext headings"},{markdown:`Foo\\
----
`,html:"<h2>Foo\\</h2>",example:90,start_line:1486,end_line:1491,section:"Setext headings"},{markdown:`\`Foo
----
\`

<a title="a lot
---
of dashes"/>
`,html:'<h2>`Foo</h2><p>`</p><h2>&lt;a title="a lot</h2><p>of dashes"/&gt;</p>',example:91,start_line:1497,end_line:1510,section:"Setext headings"},{markdown:`> Foo
---
`,html:`<blockquote>
<p>Foo</p>
</blockquote>
<hr />
`,example:92,start_line:1516,end_line:1524,section:"Setext headings"},{markdown:`> foo
bar
===
`,html:`<blockquote>
<p>foo
bar
===</p>
</blockquote>
`,example:93,start_line:1527,end_line:1537,section:"Setext headings"},{markdown:`- Foo
---
`,html:`<ul>
<li>Foo</li>
</ul>
<hr />
`,example:94,start_line:1540,end_line:1548,section:"Setext headings"},{markdown:`Foo
Bar
---
`,html:`<h2>Foo
Bar</h2>`,example:95,start_line:1555,end_line:1562,section:"Setext headings"},{markdown:`---
Foo
---
Bar
---
Baz
`,html:"<hr><h2>Foo</h2><h2>Bar</h2><p>Baz</p>",example:96,start_line:1568,end_line:1580,section:"Setext headings"},{markdown:`
====
`,html:"<p>====</p>",example:97,start_line:1585,end_line:1590,section:"Setext headings"},{markdown:`---
---
`,html:"<hr><hr>",example:98,start_line:1597,end_line:1603,section:"Setext headings"},{markdown:`- foo
-----
`,html:"<ul><li>foo</li></ul><hr>",example:99,start_line:1606,end_line:1614,section:"Setext headings"},{markdown:`    foo
---
`,html:"<pre><code>foo</code></pre><hr>",example:100,start_line:1617,end_line:1624,section:"Setext headings"},{markdown:`> foo
-----
`,html:"<blockquote><p>foo</p></blockquote><hr>",example:101,start_line:1627,end_line:1635,section:"Setext headings"},{markdown:`\\> foo
------
`,html:"<h2>&gt; foo</h2>",example:102,start_line:1641,end_line:1646,section:"Setext headings"},{markdown:`Foo

bar
---
baz
`,html:"<p>Foo</p><h2>bar</h2><p>baz</p>",example:103,start_line:1672,end_line:1682,section:"Setext headings"},{markdown:`Foo
bar

---

baz
`,html:`<p>Foo
bar</p><hr><p>baz</p>`,example:104,start_line:1688,end_line:1700,section:"Setext headings"},{markdown:`Foo
bar
* * *
baz
`,html:`<p>Foo
bar</p><hr><p>baz</p>`,example:105,start_line:1706,end_line:1716,section:"Setext headings"},{markdown:`Foo
bar
\\---
baz
`,html:`<p>Foo
bar
---
baz</p>`,example:106,start_line:1721,end_line:1731,section:"Setext headings"},{markdown:`    a simple
      indented code block
`,html:`<pre><code>a simple
  indented code block</code></pre>`,example:107,start_line:1749,end_line:1756,section:"Indented code blocks"},{markdown:`  - foo

    bar
`,html:"<ul><li><p>foo</p><p>bar</p></li></ul>",example:108,start_line:1763,end_line:1774,section:"Indented code blocks"},{markdown:`1.  foo

    - bar`,html:"<ol><li><p>foo</p><ul><li>bar</li></ul></li></ol>",example:109,start_line:1777,end_line:1790,section:"Indented code blocks"},{markdown:`    <a/>
    *hi*

    - one
`,html:`<pre><code>&lt;a/&gt;
*hi*

- one</code></pre>`,example:110,start_line:1797,end_line:1808,section:"Indented code blocks"},{markdown:`    chunk1

    chunk2
  
 
 
    chunk3
`,html:`<pre><code>chunk1

chunk2



chunk3</code></pre>`,example:111,start_line:1813,end_line:1830,section:"Indented code blocks"},{markdown:`    chunk1
      
      chunk2
`,html:`<pre><code>chunk1
  
  chunk2</code></pre>`,example:112,start_line:1836,end_line:1845,section:"Indented code blocks"},{markdown:`Foo
    bar

`,html:`<p>Foo
bar</p>`,example:113,start_line:1851,end_line:1858,section:"Indented code blocks"},{markdown:`    foo
bar
`,html:"<pre><code>foo</code></pre><p>bar</p>",example:114,start_line:1865,end_line:1872,section:"Indented code blocks"},{markdown:`# Heading
    foo
Heading
------
    foo
----
`,html:"<h1>Heading</h1><pre><code>foo</code></pre><h2>Heading</h2><pre><code>foo</code></pre><hr>",example:115,start_line:1878,end_line:1893,section:"Indented code blocks"},{markdown:`        foo
    bar
`,html:`<pre><code>    foo
bar</code></pre>`,example:116,start_line:1898,end_line:1905,section:"Indented code blocks"},{markdown:`
    
    foo
    

`,html:"<pre><code>foo</code></pre>",example:117,start_line:1911,end_line:1920,section:"Indented code blocks"},{markdown:`    foo  
`,html:"<pre><code>foo  </code></pre>",example:118,start_line:1925,end_line:1930,section:"Indented code blocks"},{markdown:"```\n<\n >\n```\n",html:`<pre><code>&lt;
 &gt;</code></pre>`,example:119,start_line:1980,end_line:1989,section:"Fenced code blocks"},{markdown:`~~~
<
 >
~~~
`,html:`<pre><code>&lt;
 &gt;</code></pre>`,example:120,start_line:1994,end_line:2003,section:"Fenced code blocks"},{markdown:"``\nfoo\n``\n",html:"<p><code>foo</code></p>",example:121,start_line:2007,end_line:2013,section:"Fenced code blocks"},{markdown:"```\naaa\n~~~\n```\n",html:`<pre><code>aaa
~~~</code></pre>`,example:122,start_line:2018,end_line:2027,section:"Fenced code blocks"},{markdown:`~~~
aaa
\`\`\`
~~~
`,html:"<pre><code>aaa\n```</code></pre>",example:123,start_line:2030,end_line:2039,section:"Fenced code blocks"},{markdown:"````\naaa\n```\n``````\n",html:"<pre><code>aaa\n```</code></pre>",example:124,start_line:2044,end_line:2053,section:"Fenced code blocks"},{markdown:`~~~~
aaa
~~~
~~~~
`,html:`<pre><code>aaa
~~~</code></pre>`,example:125,start_line:2056,end_line:2065,section:"Fenced code blocks"},{markdown:"```\n",html:"<pre><code></code></pre>",example:126,start_line:2071,end_line:2075,section:"Fenced code blocks"},{markdown:"`````\n\n```\naaa\n",html:"<pre><code>```\naaa</code></pre>",example:127,start_line:2078,end_line:2088,section:"Fenced code blocks"},{markdown:`> \`\`\`
> aaa

bbb
`,html:"<blockquote><pre><code>aaa</code></pre></blockquote><p>bbb</p>",example:128,start_line:2091,end_line:2102,section:"Fenced code blocks"},{markdown:"```\n\n  \n```\n",html:"<pre><code>  </code></pre>",example:129,start_line:2107,end_line:2116,section:"Fenced code blocks"},{markdown:"```\n```\n",html:"<pre><code></code></pre>",example:130,start_line:2121,end_line:2126,section:"Fenced code blocks"},{markdown:" ```\n aaa\naaa\n```\n",html:`<pre><code>aaa
aaa
</code></pre>
`,example:131,start_line:2133,end_line:2142,section:"Fenced code blocks"},{markdown:"  ```\naaa\n  aaa\naaa\n  ```\n",html:`<pre><code>aaa
aaa
aaa
</code></pre>
`,example:132,start_line:2145,end_line:2156,section:"Fenced code blocks"},{markdown:"   ```\n   aaa\n    aaa\n  aaa\n   ```\n",html:`<pre><code>aaa
 aaa
aaa
</code></pre>
`,example:133,start_line:2159,end_line:2170,section:"Fenced code blocks"},{markdown:"    ```\n    aaa\n    ```\n",html:"<pre><code>```\naaa\n```</code></pre>",example:134,start_line:2175,end_line:2184,section:"Fenced code blocks"},{markdown:"```\naaa\n  ```\n",html:"<pre><code>aaa</code></pre>",example:135,start_line:2190,end_line:2197,section:"Fenced code blocks"},{markdown:"   ```\naaa\n  ```\n",html:"<pre><code>aaa</code></pre>",example:136,start_line:2200,end_line:2207,section:"Fenced code blocks"},{markdown:"```\naaa\n    ```\n",html:"<pre><code>aaa\n    ```</code></pre>",example:137,start_line:2212,end_line:2220,section:"Fenced code blocks"},{markdown:"``` ```\naaa\n",html:`<p><code> </code>
aaa</p>`,example:138,start_line:2226,end_line:2232,section:"Fenced code blocks"},{markdown:`~~~~~~
aaa
~~~ ~~
`,html:`<pre><code>aaa
~~~ ~~</code></pre>`,example:139,start_line:2235,end_line:2243,section:"Fenced code blocks"},{markdown:"foo\n```\nbar\n```\nbaz\n",html:"<p>foo</p><pre><code>bar</code></pre><p>baz</p>",example:140,start_line:2249,end_line:2260,section:"Fenced code blocks"},{markdown:`foo
---
~~~
bar
~~~
# baz
`,html:"<h2>foo</h2><pre><code>bar</code></pre><h1>baz</h1>",example:141,start_line:2266,end_line:2278,section:"Fenced code blocks"},{markdown:"```ruby\ndef foo(x)\n  return 3\nend\n```\n",html:`<pre><code class="language-ruby">def foo(x)
  return 3
end</code></pre>`,example:142,start_line:2288,end_line:2299,section:"Fenced code blocks"},{markdown:`~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~
`,html:`<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>
`,example:143,start_line:2302,end_line:2313,section:"Fenced code blocks"},{markdown:"````;\n````\n",html:'<pre><code class="language-;"></code></pre>',example:144,start_line:2316,end_line:2321,section:"Fenced code blocks"},{markdown:"``` aa ```\nfoo\n",html:`<p><code>aa</code>
foo</p>`,example:145,start_line:2326,end_line:2332,section:"Fenced code blocks"},{markdown:"~~~ aa ``` ~~~\nfoo\n~~~\n",html:`<pre><code class="language-aa">foo
</code></pre>
`,example:146,start_line:2337,end_line:2344,section:"Fenced code blocks"},{markdown:"```\n``` aaa\n```\n",html:"<pre><code>``` aaa</code></pre>",example:147,start_line:2349,end_line:2356,section:"Fenced code blocks"},{markdown:`<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>
`,html:`<table><tr><td>
<pre>
**Hello**,
<p><em>world</em>.
</pre></p>
</td></tr></table>
`,example:148,start_line:2428,end_line:2443,section:"HTML blocks"},{markdown:`<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>

okay.
`,html:`<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>
<p>okay.</p>
`,example:149,start_line:2457,end_line:2476,section:"HTML blocks"},{markdown:` <div>
  *hello*
         <foo><a>
`,html:` <div>
  *hello*
         <foo><a>
`,example:150,start_line:2479,end_line:2487,section:"HTML blocks"},{markdown:`</div>
*foo*
`,html:`</div>
*foo*
`,example:151,start_line:2492,end_line:2498,section:"HTML blocks"},{markdown:`<DIV CLASS="foo">

*Markdown*

</DIV>
`,html:`<DIV CLASS="foo">
<p><em>Markdown</em></p>
</DIV>
`,example:152,start_line:2503,end_line:2513,section:"HTML blocks"},{markdown:`<div id="foo"
  class="bar">
</div>
`,html:`<div id="foo"
  class="bar">
</div>
`,example:153,start_line:2519,end_line:2527,section:"HTML blocks"},{markdown:`<div id="foo" class="bar
  baz">
</div>
`,html:`<div id="foo" class="bar
  baz">
</div>
`,example:154,start_line:2530,end_line:2538,section:"HTML blocks"},{markdown:`<div>
*foo*

*bar*
`,html:`<div>
*foo*
<p><em>bar</em></p>
`,example:155,start_line:2542,end_line:2551,section:"HTML blocks"},{markdown:`<div id="foo"
*hi*
`,html:`<div id="foo"
*hi*
`,example:156,start_line:2558,end_line:2564,section:"HTML blocks"},{markdown:`<div class
foo
`,html:`<div class
foo
`,example:157,start_line:2567,end_line:2573,section:"HTML blocks"},{markdown:`<div *???-&&&-<---
*foo*
`,html:`<div *???-&&&-<---
*foo*
`,example:158,start_line:2579,end_line:2585,section:"HTML blocks"},{markdown:`<div><a href="bar">*foo*</a></div>
`,html:`<div><a href="bar">*foo*</a></div>
`,example:159,start_line:2591,end_line:2595,section:"HTML blocks"},{markdown:`<table><tr><td>
foo
</td></tr></table>
`,html:`<table><tr><td>
foo
</td></tr></table>
`,example:160,start_line:2598,end_line:2606,section:"HTML blocks"},{markdown:"<div></div>\n``` c\nint x = 33;\n```\n",html:"<div></div>\n``` c\nint x = 33;\n```\n",example:161,start_line:2615,end_line:2625,section:"HTML blocks"},{markdown:`<a href="foo">
*bar*
</a>
`,html:`<a href="foo">
*bar*
</a>
`,example:162,start_line:2632,end_line:2640,section:"HTML blocks"},{markdown:`<Warning>
*bar*
</Warning>
`,html:`<warning>
*bar*
</warning>
`,example:163,start_line:2645,end_line:2653,section:"HTML blocks"},{markdown:`<i class="foo">
*bar*
</i>
`,html:`<i class="foo">
*bar*
</i>
`,example:164,start_line:2656,end_line:2664,section:"HTML blocks"},{markdown:`</ins>
*bar*
`,html:`</ins>
*bar*
`,example:165,start_line:2667,end_line:2673,section:"HTML blocks"},{markdown:`<del>
*foo*
</del>
`,html:`<del>
*foo*
</del>
`,example:166,start_line:2682,end_line:2690,section:"HTML blocks"},{markdown:`<del>

*foo*

</del>
`,html:`<del>
<p><em>foo</em></p>
</del>
`,example:167,start_line:2697,end_line:2707,section:"HTML blocks"},{markdown:`<del>*foo*</del>
`,html:"<p><del><em>foo</em></del></p>",example:168,start_line:2715,end_line:2719,section:"HTML blocks"},{markdown:`<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay
`,html:`<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
<p>okay</p>`,example:169,start_line:2731,end_line:2747,section:"HTML blocks"},{markdown:`<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
<\/script>
okay
`,html:`<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
<\/script>
<p>okay</p>`,example:170,start_line:2752,end_line:2766,section:"HTML blocks"},{markdown:`<textarea>

*foo*

_bar_

</textarea>
`,html:`<textarea>
*foo*

_bar_

</textarea>
`,example:171,start_line:2771,end_line:2787,section:"HTML blocks"},{markdown:`<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
okay
`,html:`<style type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
<p>okay</p>`,example:172,start_line:2791,end_line:2807,section:"HTML blocks"},{markdown:`<style
  type="text/css">

foo
`,html:`<style type="text/css">

foo


</style>`,example:173,start_line:2814,end_line:2824,section:"HTML blocks"},{markdown:`> <div>
> foo

bar
`,html:`<blockquote>
<div>
foo
</blockquote>
<p>bar</p>
`,example:174,start_line:2827,end_line:2838,section:"HTML blocks"},{markdown:`- <div>
- foo
`,html:`<ul>
<li>
<div>
</li>
<li>foo</li>
</ul>
`,example:175,start_line:2841,end_line:2851,section:"HTML blocks"},{markdown:`<style>p{color:red;}</style>
*foo*
`,html:`<style>p{color:red;}</style>
<p><em>foo</em></p>`,example:176,start_line:2856,end_line:2862,section:"HTML blocks"},{markdown:`<!-- foo -->*bar*
*baz*
`,html:`<!-- foo -->*bar*
<p><em>baz</em></p>
`,example:177,start_line:2865,end_line:2871,section:"HTML blocks"},{markdown:`<script>
foo
<\/script>1. *bar*
`,html:`<script>
foo
<\/script>1. *bar*
`,example:178,start_line:2877,end_line:2885,section:"HTML blocks"},{markdown:`<!-- Foo

bar
   baz -->
okay
`,html:`<!-- Foo

bar
   baz -->
<p>okay</p>
`,example:179,start_line:2890,end_line:2902,section:"HTML blocks"},{markdown:`<?php

  echo '>';

?>
okay
`,html:`<?php

  echo '>';

?>
<p>okay</p>
`,example:180,start_line:2908,end_line:2922,section:"HTML blocks"},{markdown:`<!DOCTYPE html>
`,html:`<!DOCTYPE html>
`,example:181,start_line:2927,end_line:2931,section:"HTML blocks"},{markdown:`<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
okay
`,html:`<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
<p>okay</p>
`,example:182,start_line:2936,end_line:2964,section:"HTML blocks"},{markdown:`  <!-- foo -->

    <!-- foo -->
`,html:`  <!-- foo -->
<pre><code>&lt;!-- foo --&gt;
</code></pre>
`,example:183,start_line:2970,end_line:2978,section:"HTML blocks"},{markdown:`  <div>

    <div>
`,html:`  <div>
<pre><code>&lt;div&gt;
</code></pre>
`,example:184,start_line:2981,end_line:2989,section:"HTML blocks"},{markdown:`Foo
<div>
bar
</div>
`,html:`<p>Foo</p>
<div>
bar
</div>
`,example:185,start_line:2995,end_line:3005,section:"HTML blocks"},{markdown:`<div>
bar
</div>
*foo*
`,html:`<div>
bar
</div>
*foo*
`,example:186,start_line:3012,end_line:3022,section:"HTML blocks"},{markdown:`Foo
<a href="bar">
baz
`,html:`<p>Foo<a href="bar">
baz
</a></p>`,example:187,start_line:3027,end_line:3035,section:"HTML blocks"},{markdown:`<div>

*Emphasized* text.

</div>
`,html:`<div>
<p><em>Emphasized</em> text.</p>
</div>
`,example:188,start_line:3068,end_line:3078,section:"HTML blocks"},{markdown:`<div>
*Emphasized* text.
</div>
`,html:`<div>
*Emphasized* text.
</div>
`,example:189,start_line:3081,end_line:3089,section:"HTML blocks"},{markdown:`<table>

<tr>

<td>
Hi
</td>

</tr>

</table>
`,html:`<table>
<tr>
<td>
Hi
</td>
</tr>
</table>
`,example:190,start_line:3103,end_line:3123,section:"HTML blocks"},{markdown:`<table>

  <tr>

    <td>
      Hi
    </td>

  </tr>

</table>
`,html:`<table>
  <tr>
<pre><code>&lt;td&gt;
  Hi
&lt;/td&gt;
</code></pre>
  </tr>
</table>
`,example:191,start_line:3130,end_line:3151,section:"HTML blocks"},{markdown:`[foo]: /url "title"

[foo]
`,html:'<p><a href="/url" title="title">foo</a></p>',example:192,start_line:3179,end_line:3185,section:"Link reference definitions"},{markdown:`   [foo]: 
      /url  
           'the title'  

[foo]
`,html:'<p><a href="/url" title="the title">foo</a></p>',example:193,start_line:3188,end_line:3196,section:"Link reference definitions"},{markdown:`[Foo*bar\\]]:my_(url) 'title (with parens)'

[Foo*bar\\]]
`,html:`<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>
`,example:194,start_line:3199,end_line:3205,section:"Link reference definitions"},{markdown:`[Foo bar]:
<my url>
'title'

[Foo bar]
`,html:'<p><a href="my url" title="title">Foo bar</a></p>',example:195,start_line:3208,end_line:3216,section:"Link reference definitions"},{markdown:`[foo]: /url '
title
line1
line2
'

[foo]
`,html:`<p><a href="/url" title="title
line1
line2
">foo</a></p>`,example:196,start_line:3221,end_line:3235,section:"Link reference definitions"},{markdown:`[foo]: /url 'title

with blank line'

[foo]
`,html:"<p>[foo]: /url 'title</p><p>with blank line'</p><p><a>foo</a></p>",example:197,start_line:3240,end_line:3250,section:"Link reference definitions"},{markdown:`[foo]:
/url

[foo]
`,html:'<p><a href="/url">foo</a></p>',example:198,start_line:3255,end_line:3262,section:"Link reference definitions"},{markdown:`[foo]:

[foo]
`,html:"<p>[foo]:</p><p><a>foo</a></p>",example:199,start_line:3267,end_line:3274,section:"Link reference definitions"},{markdown:`[foo]: <>

[foo]
`,html:"<p><a>foo</a></p>",example:200,start_line:3279,end_line:3285,section:"Link reference definitions"},{markdown:`[foo]: <bar>(baz)

[foo]
`,html:"<p>[foo]: <bar></bar>(baz)</p><p><a>foo</a></p>",example:201,start_line:3290,end_line:3297,section:"Link reference definitions"},{markdown:`[foo]: /url\\bar\\*baz "foo\\"bar\\baz"

[foo]
`,html:'<p><a href="/url\\bar*baz" title="foo&quot;bar\\baz">foo</a></p>',example:202,start_line:3303,end_line:3309,section:"Link reference definitions"},{markdown:`[foo]

[foo]: url
`,html:'<p><a href="url">foo</a></p>',example:203,start_line:3314,end_line:3320,section:"Link reference definitions"},{markdown:`[foo]

[foo]: first
[foo]: second
`,html:'<p><a href="first">foo</a></p>',example:204,start_line:3326,end_line:3333,section:"Link reference definitions"},{markdown:`[FOO]: /url

[Foo]
`,html:'<p><a href="/url">Foo</a></p>',example:205,start_line:3339,end_line:3345,section:"Link reference definitions"},{markdown:`[\u0391\u0393\u03A9]: /\u03C6\u03BF\u03C5

[\u03B1\u03B3\u03C9]
`,html:'<p><a href="/\u03C6\u03BF\u03C5">\u03B1\u03B3\u03C9</a></p>',example:206,start_line:3348,end_line:3354,section:"Link reference definitions"},{markdown:`[foo]: /url
`,html:"",example:207,start_line:3363,end_line:3366,section:"Link reference definitions"},{markdown:`[
foo
]: /url
bar
`,html:"<p>bar</p>",example:208,start_line:3371,end_line:3378,section:"Link reference definitions"},{markdown:`[foo]: /url "title" ok
`,html:'<p>[foo]: /url "title" ok</p>',example:209,start_line:3384,end_line:3388,section:"Link reference definitions"},{markdown:`[foo]: /url
"title" ok
`,html:'<p>"title" ok</p>',example:210,start_line:3393,end_line:3398,section:"Link reference definitions"},{markdown:`    [foo]: /url "title"

[foo]
`,html:'<pre><code>[foo]: /url "title"</code></pre><p><a>foo</a></p>',example:211,start_line:3404,end_line:3412,section:"Link reference definitions"},{markdown:"```\n[foo]: /url\n```\n\n[foo]\n",html:"<pre><code>[foo]: /url</code></pre><p><a>foo</a></p>",example:212,start_line:3418,end_line:3428,section:"Link reference definitions"},{markdown:`Foo
[bar]: /baz

[bar]
`,html:`<p>Foo
[bar]: /baz</p><p><a>bar</a></p>`,example:213,start_line:3433,end_line:3442,section:"Link reference definitions"},{markdown:`# [Foo]
[foo]: /url
> bar
`,html:'<h1><a href="/url">Foo</a></h1><blockquote><p>bar</p></blockquote>',example:214,start_line:3448,end_line:3457,section:"Link reference definitions"},{markdown:`[foo]: /url
bar
===
[foo]
`,html:'<h1>bar</h1><p><a href="/url">foo</a></p>',example:215,start_line:3459,end_line:3467,section:"Link reference definitions"},{markdown:`[foo]: /url
===
[foo]
`,html:`<p>===
<a href="/url">foo</a></p>`,example:216,start_line:3469,end_line:3476,section:"Link reference definitions"},{markdown:`[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]
`,html:`<p><a href="/foo-url" title="foo">foo</a>,
<a href="/bar-url" title="bar">bar</a>,
<a href="/baz-url">baz</a></p>`,example:217,start_line:3482,end_line:3495,section:"Link reference definitions"},{markdown:`[foo]

> [foo]: /url
`,html:'<p><a href="/url">foo</a></p><blockquote></blockquote>',example:218,start_line:3503,end_line:3511,section:"Link reference definitions"},{markdown:`aaa

bbb
`,html:"<p>aaa</p><p>bbb</p>",example:219,start_line:3525,end_line:3532,section:"Paragraphs"},{markdown:`aaa
bbb

ccc
ddd
`,html:`<p>aaa
bbb</p><p>ccc
ddd</p>`,example:220,start_line:3537,end_line:3548,section:"Paragraphs"},{markdown:`aaa


bbb
`,html:"<p>aaa</p><p>bbb</p>",example:221,start_line:3553,end_line:3561,section:"Paragraphs"},{markdown:`  aaa
 bbb
`,html:`<p>aaa
bbb</p>`,example:222,start_line:3566,end_line:3572,section:"Paragraphs"},{markdown:`aaa
             bbb
                                       ccc
`,html:`<p>aaa
bbb
ccc</p>`,example:223,start_line:3578,end_line:3586,section:"Paragraphs"},{markdown:`   aaa
bbb
`,html:`<p>aaa
bbb</p>`,example:224,start_line:3592,end_line:3598,section:"Paragraphs"},{markdown:`    aaa
bbb
`,html:"<pre><code>aaa</code></pre><p>bbb</p>",example:225,start_line:3601,end_line:3608,section:"Paragraphs"},{markdown:`aaa     
bbb     
`,html:`<p>aaa<br />
bbb</p>
`,example:226,start_line:3615,end_line:3621,section:"Paragraphs"},{markdown:`  

aaa
  

# aaa

  
`,html:"<p>aaa</p><h1>aaa</h1>",example:227,start_line:3632,end_line:3644,section:"Blank lines"},{markdown:`> # Foo
> bar
> baz
`,html:`<blockquote><h1>Foo</h1><p>bar
baz</p></blockquote>`,example:228,start_line:3700,end_line:3710,section:"Block quotes"},{markdown:`># Foo
>bar
> baz
`,html:`<blockquote><h1>Foo</h1><p>bar
baz</p></blockquote>`,example:229,start_line:3715,end_line:3725,section:"Block quotes"},{markdown:`   > # Foo
   > bar
 > baz
`,html:`<blockquote><h1>Foo</h1><p>bar
baz</p></blockquote>`,example:230,start_line:3730,end_line:3740,section:"Block quotes"},{markdown:`    > # Foo
    > bar
    > baz
`,html:`<pre><code>&gt; # Foo
&gt; bar
&gt; baz</code></pre>`,example:231,start_line:3745,end_line:3754,section:"Block quotes"},{markdown:`> # Foo
> bar
baz
`,html:`<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>
`,example:232,start_line:3760,end_line:3770,section:"Block quotes"},{markdown:`> bar
baz
> foo
`,html:`<blockquote>
<p>bar
baz
foo</p>
</blockquote>
`,example:233,start_line:3776,end_line:3786,section:"Block quotes"},{markdown:`> foo
---
`,html:"<blockquote><p>foo</p></blockquote><hr>",example:234,start_line:3800,end_line:3808,section:"Block quotes"},{markdown:`> - foo
- bar`,html:"<blockquote><ul><li>foo</li></ul></blockquote><ul><li>bar</li></ul>",example:235,start_line:3820,end_line:3832,section:"Block quotes"},{markdown:`>     foo
    bar
`,html:"<blockquote><pre><code>foo</code></pre></blockquote><pre><code>bar</code></pre>",example:236,start_line:3838,end_line:3848,section:"Block quotes"},{markdown:"> ```\nfoo\n```\n",html:"<blockquote><pre><code></code></pre></blockquote><p>foo</p><pre><code></code></pre>",example:237,start_line:3851,end_line:3861,section:"Block quotes"},{markdown:`> foo
    - bar
`,html:`<blockquote>
<p>foo
- bar</p>
</blockquote>
`,example:238,start_line:3867,end_line:3875,section:"Block quotes"},{markdown:`>
`,html:"<blockquote></blockquote>",example:239,start_line:3891,end_line:3896,section:"Block quotes"},{markdown:`>
>  
> 
`,html:"<blockquote></blockquote>",example:240,start_line:3899,end_line:3906,section:"Block quotes"},{markdown:`>
> foo
>  
`,html:"<blockquote><p>foo</p></blockquote>",example:241,start_line:3911,end_line:3919,section:"Block quotes"},{markdown:`> foo

> bar
`,html:"<blockquote><p>foo</p></blockquote><blockquote><p>bar</p></blockquote>",example:242,start_line:3924,end_line:3935,section:"Block quotes"},{markdown:`> foo
> bar
`,html:`<blockquote><p>foo
bar</p></blockquote>`,example:243,start_line:3946,end_line:3954,section:"Block quotes"},{markdown:`> foo
>
> bar
`,html:"<blockquote><p>foo</p><p>bar</p></blockquote>",example:244,start_line:3959,end_line:3968,section:"Block quotes"},{markdown:`foo
> bar
`,html:"<p>foo</p><blockquote><p>bar</p></blockquote>",example:245,start_line:3973,end_line:3981,section:"Block quotes"},{markdown:`> aaa
***
> bbb
`,html:"<blockquote><p>aaa</p></blockquote><hr><blockquote><p>bbb</p></blockquote>",example:246,start_line:3987,end_line:3999,section:"Block quotes"},{markdown:`> bar
baz
`,html:`<blockquote>
<p>bar
baz</p>
</blockquote>
`,example:247,start_line:4005,end_line:4013,section:"Block quotes"},{markdown:`> bar

baz
`,html:"<blockquote><p>bar</p></blockquote><p>baz</p>",example:248,start_line:4016,end_line:4025,section:"Block quotes"},{markdown:`> bar
>
baz
`,html:"<blockquote><p>bar</p></blockquote><p>baz</p>",example:249,start_line:4028,end_line:4037,section:"Block quotes"},{markdown:`> > > foo
bar
`,html:`<blockquote>
<blockquote>
<blockquote>
<p>foo
bar</p>
</blockquote>
</blockquote>
</blockquote>
`,example:250,start_line:4044,end_line:4056,section:"Block quotes"},{markdown:`>>> foo
> bar
>>baz
`,html:`<blockquote>
<blockquote>
<blockquote>
<p>foo
bar
baz</p>
</blockquote>
</blockquote>
</blockquote>
`,example:251,start_line:4059,end_line:4073,section:"Block quotes"},{markdown:`>     code

>    not code
`,html:"<blockquote><pre><code>code</code></pre></blockquote><blockquote><p>not code</p></blockquote>",example:252,start_line:4081,end_line:4093,section:"Block quotes"},{markdown:`A paragraph
with two lines.

    indented code

> A block quote.
`,html:`<p>A paragraph
with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote>`,example:253,start_line:4135,end_line:4150,section:"List items"},{markdown:`1.  A paragraph
    with two lines.

        indented code

    > A block quote.
`,html:`<ol><li><p>A paragraph
with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>`,example:254,start_line:4157,end_line:4176,section:"List items"},{markdown:`- one
 two
`,html:"<ul><li>one</li></ul><p>two</p>",example:255,start_line:4190,end_line:4199,section:"List items"},{markdown:`- one

  two
`,html:"<ul><li><p>one</p><p>two</p></li></ul>",example:256,start_line:4202,end_line:4213,section:"List items"},{markdown:` -    one
     two
`,html:"<ul><li>one</li></ul><pre><code> two</code></pre>",example:257,start_line:4216,end_line:4226,section:"List items"},{markdown:` -    one

      two
`,html:"<ul><li><p>one</p><p>two</p></li></ul>",example:258,start_line:4229,end_line:4240,section:"List items"},{markdown:`   > > 1.  one
>>
>>     two
`,html:"<blockquote><blockquote><ol><li><p>one</p><p>two</p></li></ol></blockquote></blockquote>",example:259,start_line:4251,end_line:4266,section:"List items"},{markdown:`>>- one
  >  > two
`,html:"<blockquote><blockquote><ul><li>one</li></ul><p>two</p></blockquote></blockquote>",example:260,start_line:4278,end_line:4291,section:"List items"},{markdown:`-one

2.two
`,html:"<p>-one</p><p>2.two</p>",example:261,start_line:4297,end_line:4304,section:"List items"},{markdown:`- foo


  bar
`,html:"<ul><li><p>foo</p><p>bar</p></li></ul>",example:262,start_line:4310,end_line:4322,section:"List items"},{markdown:`1.  foo

    \`\`\`
    bar
    \`\`\`

    baz

    > bam
`,html:"<ol><li><p>foo</p><pre><code>bar</code></pre><p>baz</p><blockquote><p>bam</p></blockquote></li></ol>",example:263,start_line:4327,end_line:4349,section:"List items"},{markdown:`- Foo

      bar


      baz
`,html:`<ul><li><p>Foo</p><pre><code>bar


baz</code></pre></li></ul>`,example:264,start_line:4355,end_line:4373,section:"List items"},{markdown:"123456789. ok",html:'<ol start="123456789"><li>ok</li></ol>',example:265,start_line:4377,end_line:4383,section:"List items"},{markdown:`1234567890. not ok
`,html:"<p>1234567890. not ok</p>",example:266,start_line:4386,end_line:4390,section:"List items"},{markdown:"0. ok",html:'<ol start="0"><li>ok</li></ol>',example:267,start_line:4395,end_line:4401,section:"List items"},{markdown:"003. ok",html:'<ol start="3"><li>ok</li></ol>',example:268,start_line:4404,end_line:4410,section:"List items"},{markdown:`-1. not ok
`,html:"<p>-1. not ok</p>",example:269,start_line:4415,end_line:4419,section:"List items"},{markdown:`- foo

      bar
`,html:"<ul><li><p>foo</p><pre><code>bar</code></pre></li></ul>",example:270,start_line:4438,end_line:4450,section:"List items"},{markdown:`  10.  foo

           bar
`,html:'<ol start="10"><li><p>foo</p><pre><code>bar</code></pre></li></ol>',example:271,start_line:4455,end_line:4467,section:"List items"},{markdown:`    indented code

paragraph

    more code
`,html:"<pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre>",example:272,start_line:4474,end_line:4486,section:"List items"},{markdown:`1.     indented code

   paragraph

       more code
`,html:"<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>",example:273,start_line:4489,end_line:4505,section:"List items"},{markdown:`1.      indented code

   paragraph

       more code
`,html:"<ol><li><pre><code> indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>",example:274,start_line:4511,end_line:4527,section:"List items"},{markdown:`   foo

bar
`,html:"<p>foo</p><p>bar</p>",example:275,start_line:4538,end_line:4545,section:"List items"},{markdown:`-    foo
  bar
`,html:"<ul><li>foo</li></ul><p>bar</p>",example:276,start_line:4548,end_line:4557,section:"List items"},{markdown:`-  foo

   bar
`,html:"<ul><li><p>foo</p><p>bar</p></li></ul>",example:277,start_line:4565,end_line:4576,section:"List items"},{markdown:`-
  foo
-
  \`\`\`
  bar
  \`\`\`
-
      baz
`,html:"<ul><li>foo</li><li><pre><code>bar</code></pre></li><li><pre><code>baz</code></pre></li></ul>",example:278,start_line:4592,end_line:4613,section:"List items"},{markdown:`-   
  foo`,html:"<ul><li>foo</li></ul>",example:279,start_line:4618,end_line:4625,section:"List items"},{markdown:`-

  foo
`,html:"<ul><li></li></ul><p>foo</p>",example:280,start_line:4632,end_line:4641,section:"List items"},{markdown:`- foo
-
- bar`,html:"<ul><li>foo</li><li></li><li>bar</li></ul>",example:281,start_line:4646,end_line:4656,section:"List items"},{markdown:`- foo
-   
- bar`,html:"<ul><li>foo</li><li></li><li>bar</li></ul>",example:282,start_line:4661,end_line:4671,section:"List items"},{markdown:`1. foo
2.
3. bar`,html:"<ol><li>foo</li><li></li><li>bar</li></ol>",example:283,start_line:4676,end_line:4686,section:"List items"},{markdown:`*
`,html:"<ul><li></li></ul>",example:284,start_line:4691,end_line:4697,section:"List items"},{markdown:`foo
*

foo
1.
`,html:`<p>foo
*</p><p>foo
1.</p>`,example:285,start_line:4701,end_line:4712,section:"List items"},{markdown:` 1.  A paragraph
     with two lines.

         indented code

     > A block quote.
`,html:`<ol><li><p>A paragraph
with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>`,example:286,start_line:4723,end_line:4742,section:"List items"},{markdown:`  1.  A paragraph
      with two lines.

          indented code

      > A block quote.
`,html:`<ol><li><p>A paragraph
with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>`,example:287,start_line:4747,end_line:4766,section:"List items"},{markdown:`   1.  A paragraph
       with two lines.

           indented code

       > A block quote.
`,html:`<ol><li><p>A paragraph
with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>`,example:288,start_line:4771,end_line:4790,section:"List items"},{markdown:`    1.  A paragraph
        with two lines.

            indented code

        > A block quote.
`,html:`<pre><code>1.  A paragraph
    with two lines.

        indented code

    &gt; A block quote.</code></pre>`,example:289,start_line:4795,end_line:4810,section:"List items"},{markdown:`  1.  A paragraph
with two lines.

          indented code

      > A block quote.
`,html:`<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>
`,example:290,start_line:4825,end_line:4844,section:"List items"},{markdown:`  1.  A paragraph
    with two lines.
`,html:`<ol>
<li>A paragraph
with two lines.</li>
</ol>
`,example:291,start_line:4849,end_line:4857,section:"List items"},{markdown:`> 1. > Blockquote
continued here.
`,html:`<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>
`,example:292,start_line:4862,end_line:4876,section:"List items"},{markdown:`> 1. > Blockquote
> continued here.
`,html:`<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>
`,example:293,start_line:4879,end_line:4893,section:"List items"},{markdown:`- foo
  - bar
    - baz
      - boo`,html:"<ul><li>foo<ul><li>bar<ul><li>baz<ul><li>boo</li></ul></li></ul></li></ul></li></ul>",example:294,start_line:4907,end_line:4928,section:"List items"},{markdown:`- foo
 - bar
  - baz
   - boo
`,html:`<ul>
<li>foo</li>
<li>bar</li>
<li>baz</li>
<li>boo</li>
</ul>
`,example:295,start_line:4933,end_line:4945,section:"List items"},{markdown:`10) foo
    - bar`,html:'<ol start="10"><li>foo<ul><li>bar</li></ul></li></ol>',example:296,start_line:4950,end_line:4961,section:"List items"},{markdown:`10) foo
   - bar`,html:'<ol start="10"><li>foo</li></ol><ul><li>bar</li></ul>',example:297,start_line:4966,end_line:4976,section:"List items"},{markdown:"- - foo",html:"<ul><li><ul><li>foo</li></ul></li></ul>",example:298,start_line:4981,end_line:4991,section:"List items"},{markdown:"1. - 2. foo",html:'<ol><li><ul><li><ol start="2"><li>foo</li></ol></li></ul></li></ol>',example:299,start_line:4994,end_line:5008,section:"List items"},{markdown:`- # Foo
- Bar
  ---
  baz
`,html:`<ul>
<li>
<h1>Foo</h1>
</li>
<li>
<h2>Bar</h2>
baz</li>
</ul>
`,example:300,start_line:5013,end_line:5027,section:"List items"},{markdown:`- foo
- bar
+ baz`,html:"<ul><li>foo</li><li>bar</li></ul><ul><li>baz</li></ul>",example:301,start_line:5249,end_line:5261,section:"Lists"},{markdown:`1. foo
2. bar
3) baz`,html:'<ol><li>foo</li><li>bar</li></ol><ol start="3"><li>baz</li></ol>',example:302,start_line:5264,end_line:5276,section:"Lists"},{markdown:`Foo
- bar
- baz`,html:"<p>Foo</p><ul><li>bar</li><li>baz</li></ul>",example:303,start_line:5283,end_line:5293,section:"Lists"},{markdown:`The number of windows in my house is
14.  The number of doors is 6.
`,html:`<p>The number of windows in my house is
14.  The number of doors is 6.</p>`,example:304,start_line:5360,end_line:5366,section:"Lists"},{markdown:`The number of windows in my house is
1.  The number of doors is 6.`,html:"<p>The number of windows in my house is</p><ol><li>The number of doors is 6.</li></ol>",example:305,start_line:5370,end_line:5378,section:"Lists"},{markdown:`- foo

- bar


- baz
`,html:"<ul><li><p>foo</p></li><li><p>bar</p></li><li><p>baz</p></li></ul>",example:306,start_line:5384,end_line:5403,section:"Lists"},{markdown:`- foo
  - bar
    - baz


      bim
`,html:"<ul><li>foo<ul><li>bar<ul><li><p>baz</p><p>bim</p></li></ul></li></ul></li></ul>",example:307,start_line:5405,end_line:5427,section:"Lists"},{markdown:`- foo
- bar

<!-- -->

- baz
- bim
`,html:`<ul>
<li>foo</li>
<li>bar</li>
</ul>
<!-- -->
<ul>
<li>baz</li>
<li>bim</li>
</ul>
`,example:308,start_line:5435,end_line:5453,section:"Lists"},{markdown:`-   foo

    notcode

-   foo

<!-- -->

    code
`,html:`<ul>
<li>
<p>foo</p>
<p>notcode</p>
</li>
<li>
<p>foo</p>
</li>
</ul>
<!-- -->
<pre><code>code
</code></pre>
`,example:309,start_line:5456,end_line:5479,section:"Lists"},{markdown:`- a
 - b
  - c
   - d
  - e
 - f
- g
`,html:`<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d</li>
<li>e</li>
<li>f</li>
<li>g</li>
</ul>
`,example:310,start_line:5487,end_line:5505,section:"Lists"},{markdown:`1. a

  2. b

   3. c
`,html:`<ol>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ol>
`,example:311,start_line:5508,end_line:5526,section:"Lists"},{markdown:`- a
 - b
  - c
   - d
    - e
`,html:`<ul>
<li>a</li>
<li>b</li>
<li>c</li>
<li>d
- e</li>
</ul>
`,example:312,start_line:5532,end_line:5546,section:"Lists"},{markdown:`1. a

  2. b

    3. c
`,html:`<ol>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
</ol>
<pre><code>3. c
</code></pre>
`,example:313,start_line:5552,end_line:5569,section:"Lists"},{markdown:`- a
- b

- c
`,html:`<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ul>
`,example:314,start_line:5575,end_line:5592,section:"Lists"},{markdown:`* a
*

* c
`,html:`<ul>
<li>
<p>a</p>
</li>
<li></li>
<li>
<p>c</p>
</li>
</ul>
`,example:315,start_line:5597,end_line:5612,section:"Lists"},{markdown:`- a
- b

  c
- d
`,html:`<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
<p>c</p>
</li>
<li>
<p>d</p>
</li>
</ul>
`,example:316,start_line:5619,end_line:5638,section:"Lists"},{markdown:`- a
- b

  [ref]: /url
- d
`,html:`<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>d</p>
</li>
</ul>
`,example:317,start_line:5641,end_line:5659,section:"Lists"},{markdown:"- a\n- ```\n  b\n\n\n  ```\n- c",html:"<ul><li>a</li><li><pre><code>b</code></pre></li><li>c</li></ul>",example:318,start_line:5664,end_line:5683,section:"Lists"},{markdown:`- a
  - b

    c
- d
`,html:`<ul>
<li>a
<ul>
<li>
<p>b</p>
<p>c</p>
</li>
</ul>
</li>
<li>d</li>
</ul>
`,example:319,start_line:5690,end_line:5708,section:"Lists"},{markdown:`* a
  > b
  >
* c
`,html:`<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
</li>
<li>c</li>
</ul>
`,example:320,start_line:5714,end_line:5728,section:"Lists"},{markdown:"- a\n  > b\n  ```\n  c\n  ```\n- d\n",html:`<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
<pre><code>c
</code></pre>
</li>
<li>d</li>
</ul>
`,example:321,start_line:5734,end_line:5752,section:"Lists"},{markdown:"- a",html:"<ul><li>a</li></ul>",example:322,start_line:5757,end_line:5763,section:"Lists"},{markdown:`- a
  - b`,html:"<ul><li>a<ul><li>b</li></ul></li></ul>",example:323,start_line:5766,end_line:5777,section:"Lists"},{markdown:"1. ```\n   foo\n   ```\n\n   bar\n",html:"<ol><li><pre><code>foo</code></pre><p>bar</p></li></ol>",example:324,start_line:5783,end_line:5797,section:"Lists"},{markdown:`* foo
  * bar

  baz
`,html:`<ul>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
<p>baz</p>
</li>
</ul>
`,example:325,start_line:5802,end_line:5817,section:"Lists"},{markdown:`- a
  - b
  - c

- d
  - e
  - f
`,html:`<ul>
<li>
<p>a</p>
<ul>
<li>b</li>
<li>c</li>
</ul>
</li>
<li>
<p>d</p>
<ul>
<li>e</li>
<li>f</li>
</ul>
</li>
</ul>
`,example:326,start_line:5820,end_line:5845,section:"Lists"},{markdown:"`hi`lo`\n",html:"<p><code>hi</code>lo`</p>",example:327,start_line:5854,end_line:5858,section:"Inlines"},{markdown:"`foo`\n",html:"<p><code>foo</code></p>",example:328,start_line:5886,end_line:5890,section:"Code spans"},{markdown:"`` foo ` bar ``\n",html:"<p><code>foo ` bar</code></p>",example:329,start_line:5897,end_line:5901,section:"Code spans"},{markdown:"` `` `\n",html:"<p><code>``</code></p>",example:330,start_line:5907,end_line:5911,section:"Code spans"},{markdown:"`  ``  `\n",html:"<p><code> `` </code></p>",example:331,start_line:5915,end_line:5919,section:"Code spans"},{markdown:"` a`\n",html:"<p><code> a</code></p>",example:332,start_line:5924,end_line:5928,section:"Code spans"},{markdown:"`\xA0b\xA0`\n",html:"<p><code>&nbsp;b&nbsp;</code></p>",example:333,start_line:5933,end_line:5937,section:"Code spans"},{markdown:"`\xA0`\n`  `\n",html:`<p><code>\xA0</code>
<code>  </code></p>`,example:334,start_line:5941,end_line:5947,section:"Code spans"},{markdown:`\`\`
foo
bar  
baz
\`\`
`,html:"<p><code>foo bar baz</code></p>",example:335,start_line:5952,end_line:5960,section:"Code spans"},{markdown:"``\nfoo \n``\n",html:"<p><code>foo</code></p>",example:336,start_line:5962,end_line:5968,section:"Code spans"},{markdown:"`foo   bar \nbaz`\n",html:"<p><code>foo   bar baz</code></p>",example:337,start_line:5973,end_line:5978,section:"Code spans"},{markdown:"`foo\\`bar`\n",html:"<p><code>foo\\</code>bar`</p>",example:338,start_line:5990,end_line:5994,section:"Code spans"},{markdown:"``foo`bar``\n",html:"<p><code>foo`bar</code></p>",example:339,start_line:6001,end_line:6005,section:"Code spans"},{markdown:"` foo `` bar `\n",html:"<p><code>foo `` bar</code></p>",example:340,start_line:6007,end_line:6011,section:"Code spans"},{markdown:"*foo`*`\n",html:"<p>*foo<code>*</code></p>",example:341,start_line:6019,end_line:6023,section:"Code spans"},{markdown:"[not a `link](/foo`)\n",html:"<p>[not a <code>link](/foo</code>)</p>",example:342,start_line:6028,end_line:6032,section:"Code spans"},{markdown:'`<a href="`">`\n',html:'<p><code>&lt;a href="</code>"&gt;`</p>',example:343,start_line:6038,end_line:6042,section:"Code spans"},{markdown:'<a href="`">`\n',html:'<p><a href="`"></a>`</p>',example:344,start_line:6047,end_line:6051,section:"Code spans"},{markdown:"`<https://foo.bar.`baz>`\n",html:"<p><code>&lt;https://foo.bar.</code>baz&gt;`</p>",example:345,start_line:6056,end_line:6060,section:"Code spans"},{markdown:"<https://foo.bar.`baz>`\n",html:'<p><a href="https://foo.bar.`baz">https://foo.bar.`baz</a>`</p>',example:346,start_line:6065,end_line:6069,section:"Code spans"},{markdown:"```foo``\n",html:"<p>```foo``</p>\n",example:347,start_line:6075,end_line:6079,section:"Code spans"},{markdown:"`foo\n",html:"<p>`foo</p>",example:348,start_line:6082,end_line:6086,section:"Code spans"},{markdown:"`foo``bar``\n",html:"<p>`foo<code>bar</code></p>",example:349,start_line:6091,end_line:6095,section:"Code spans"},{markdown:`*foo bar*
`,html:"<p><em>foo bar</em></p>",example:350,start_line:6308,end_line:6312,section:"Emphasis and strong emphasis"},{markdown:`a * foo bar*
`,html:"<p>a * foo bar*</p>",example:351,start_line:6318,end_line:6322,section:"Emphasis and strong emphasis"},{markdown:`a*"foo"*
`,html:'<p>a*"foo"*</p>',example:352,start_line:6329,end_line:6333,section:"Emphasis and strong emphasis"},{markdown:`*\xA0a\xA0*
`,html:"<p>*&nbsp;a&nbsp;*</p>",example:353,start_line:6338,end_line:6342,section:"Emphasis and strong emphasis"},{markdown:`*$*alpha.

*\xA3*bravo.

*\u20AC*charlie.
`,html:"<p>*$*alpha.</p><p>*\xA3*bravo.</p><p>*\u20AC*charlie.</p>",example:354,start_line:6347,end_line:6357,section:"Emphasis and strong emphasis"},{markdown:`foo*bar*
`,html:"<p>foo<em>bar</em></p>",example:355,start_line:6362,end_line:6366,section:"Emphasis and strong emphasis"},{markdown:`5*6*78
`,html:"<p>5<em>6</em>78</p>",example:356,start_line:6369,end_line:6373,section:"Emphasis and strong emphasis"},{markdown:`_foo bar_
`,html:"<p><em>foo bar</em></p>",example:357,start_line:6378,end_line:6382,section:"Emphasis and strong emphasis"},{markdown:`_ foo bar_
`,html:"<p>_ foo bar_</p>",example:358,start_line:6388,end_line:6392,section:"Emphasis and strong emphasis"},{markdown:`a_"foo"_
`,html:'<p>a_"foo"_</p>',example:359,start_line:6398,end_line:6402,section:"Emphasis and strong emphasis"},{markdown:`foo_bar_
`,html:`<p>foo_bar_</p>
`,example:360,start_line:6407,end_line:6411,section:"Emphasis and strong emphasis"},{markdown:`5_6_78
`,html:`<p>5_6_78</p>
`,example:361,start_line:6414,end_line:6418,section:"Emphasis and strong emphasis"},{markdown:`\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C_\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F_
`,html:`<p>\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C_\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F_</p>
`,example:362,start_line:6421,end_line:6425,section:"Emphasis and strong emphasis"},{markdown:`aa_"bb"_cc
`,html:'<p>aa_"bb"_cc</p>',example:363,start_line:6431,end_line:6435,section:"Emphasis and strong emphasis"},{markdown:`foo-_(bar)_
`,html:"<p>foo-<em>(bar)</em></p>",example:364,start_line:6442,end_line:6446,section:"Emphasis and strong emphasis"},{markdown:`_foo*
`,html:"<p>_foo*</p>",example:365,start_line:6454,end_line:6458,section:"Emphasis and strong emphasis"},{markdown:`*foo bar *
`,html:"<p>*foo bar *</p>",example:366,start_line:6464,end_line:6468,section:"Emphasis and strong emphasis"},{markdown:`*foo bar
*
`,html:`<p>*foo bar
*</p>`,example:367,start_line:6473,end_line:6479,section:"Emphasis and strong emphasis"},{markdown:`*(*foo)
`,html:"<p>*(*foo)</p>",example:368,start_line:6486,end_line:6490,section:"Emphasis and strong emphasis"},{markdown:`*(*foo*)*
`,html:"<p><em>(<em>foo</em>)</em></p>",example:369,start_line:6496,end_line:6500,section:"Emphasis and strong emphasis"},{markdown:`*foo*bar
`,html:"<p><em>foo</em>bar</p>",example:370,start_line:6505,end_line:6509,section:"Emphasis and strong emphasis"},{markdown:`_foo bar _
`,html:"<p>_foo bar _</p>",example:371,start_line:6518,end_line:6522,section:"Emphasis and strong emphasis"},{markdown:`_(_foo)
`,html:"<p>_(_foo)</p>",example:372,start_line:6528,end_line:6532,section:"Emphasis and strong emphasis"},{markdown:`_(_foo_)_
`,html:"<p><em>(<em>foo</em>)</em></p>",example:373,start_line:6537,end_line:6541,section:"Emphasis and strong emphasis"},{markdown:`_foo_bar
`,html:"<p>_foo_bar</p>",example:374,start_line:6546,end_line:6550,section:"Emphasis and strong emphasis"},{markdown:`_\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C_\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F
`,html:`<p>_\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C_\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F</p>
`,example:375,start_line:6553,end_line:6557,section:"Emphasis and strong emphasis"},{markdown:`_foo_bar_baz_
`,html:`<p><em>foo_bar_baz</em></p>
`,example:376,start_line:6560,end_line:6564,section:"Emphasis and strong emphasis"},{markdown:`_(bar)_.
`,html:"<p><em>(bar)</em>.</p>",example:377,start_line:6571,end_line:6575,section:"Emphasis and strong emphasis"},{markdown:`**foo bar**
`,html:"<p><strong>foo bar</strong></p>",example:378,start_line:6580,end_line:6584,section:"Emphasis and strong emphasis"},{markdown:`** foo bar**
`,html:"<p>** foo bar**</p>",example:379,start_line:6590,end_line:6594,section:"Emphasis and strong emphasis"},{markdown:`a**"foo"**
`,html:'<p>a**"foo"**</p>',example:380,start_line:6601,end_line:6605,section:"Emphasis and strong emphasis"},{markdown:`foo**bar**
`,html:"<p>foo<strong>bar</strong></p>",example:381,start_line:6610,end_line:6614,section:"Emphasis and strong emphasis"},{markdown:`__foo bar__
`,html:"<p><strong>foo bar</strong></p>",example:382,start_line:6619,end_line:6623,section:"Emphasis and strong emphasis"},{markdown:`__ foo bar__
`,html:"<p>__ foo bar__</p>",example:383,start_line:6629,end_line:6633,section:"Emphasis and strong emphasis"},{markdown:`__
foo bar__
`,html:`<p>__
foo bar__</p>`,example:384,start_line:6637,end_line:6643,section:"Emphasis and strong emphasis"},{markdown:`a__"foo"__
`,html:'<p>a__"foo"__</p>',example:385,start_line:6649,end_line:6653,section:"Emphasis and strong emphasis"},{markdown:`foo__bar__
`,html:`<p>foo__bar__</p>
`,example:386,start_line:6658,end_line:6662,section:"Emphasis and strong emphasis"},{markdown:`5__6__78
`,html:`<p>5__6__78</p>
`,example:387,start_line:6665,end_line:6669,section:"Emphasis and strong emphasis"},{markdown:`\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C__\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F__
`,html:`<p>\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C__\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F__</p>
`,example:388,start_line:6672,end_line:6676,section:"Emphasis and strong emphasis"},{markdown:`__foo, __bar__, baz__
`,html:`<p><strong>foo, <strong>bar</strong>, baz</strong></p>
`,example:389,start_line:6679,end_line:6683,section:"Emphasis and strong emphasis"},{markdown:`foo-__(bar)__
`,html:"<p>foo-<strong>(bar)</strong></p>",example:390,start_line:6690,end_line:6694,section:"Emphasis and strong emphasis"},{markdown:`**foo bar **
`,html:"<p>**foo bar **</p>",example:391,start_line:6703,end_line:6707,section:"Emphasis and strong emphasis"},{markdown:`**(**foo)
`,html:`<p>**(**foo)</p>
`,example:392,start_line:6716,end_line:6720,section:"Emphasis and strong emphasis"},{markdown:`*(**foo**)*
`,html:`<p><em>(<strong>foo</strong>)</em></p>
`,example:393,start_line:6726,end_line:6730,section:"Emphasis and strong emphasis"},{markdown:`**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**
`,html:`<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.
<em>Asclepias physocarpa</em>)</strong></p>`,example:394,start_line:6733,end_line:6739,section:"Emphasis and strong emphasis"},{markdown:`**foo "*bar*" foo**
`,html:'<p><strong>foo "<em>bar</em>" foo</strong></p>',example:395,start_line:6742,end_line:6746,section:"Emphasis and strong emphasis"},{markdown:`**foo**bar
`,html:"<p><strong>foo</strong>bar</p>",example:396,start_line:6751,end_line:6755,section:"Emphasis and strong emphasis"},{markdown:`__foo bar __
`,html:"<p>__foo bar __</p>",example:397,start_line:6763,end_line:6767,section:"Emphasis and strong emphasis"},{markdown:`__(__foo)
`,html:`<p>__(__foo)</p>
`,example:398,start_line:6773,end_line:6777,section:"Emphasis and strong emphasis"},{markdown:`_(__foo__)_
`,html:`<p><em>(<strong>foo</strong>)</em></p>
`,example:399,start_line:6783,end_line:6787,section:"Emphasis and strong emphasis"},{markdown:`__foo__bar
`,html:`<p>__foo__bar</p>
`,example:400,start_line:6792,end_line:6796,section:"Emphasis and strong emphasis"},{markdown:`__\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C__\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F
`,html:`<p>__\u043F\u0440\u0438\u0441\u0442\u0430\u043D\u044F\u043C__\u0441\u0442\u0440\u0435\u043C\u044F\u0442\u0441\u044F</p>
`,example:401,start_line:6799,end_line:6803,section:"Emphasis and strong emphasis"},{markdown:`__foo__bar__baz__
`,html:`<p><strong>foo__bar__baz</strong></p>
`,example:402,start_line:6806,end_line:6810,section:"Emphasis and strong emphasis"},{markdown:`__(bar)__.
`,html:"<p><strong>(bar)</strong>.</p>",example:403,start_line:6817,end_line:6821,section:"Emphasis and strong emphasis"},{markdown:`*foo [bar](/url)*
`,html:'<p><em>foo <a href="/url">bar</a></em></p>',example:404,start_line:6829,end_line:6833,section:"Emphasis and strong emphasis"},{markdown:`*foo
bar*
`,html:`<p><em>foo
bar</em></p>`,example:405,start_line:6836,end_line:6842,section:"Emphasis and strong emphasis"},{markdown:`_foo __bar__ baz_
`,html:"<p><em>foo <strong>bar</strong> baz</em></p>",example:406,start_line:6848,end_line:6852,section:"Emphasis and strong emphasis"},{markdown:`_foo _bar_ baz_
`,html:"<p><em>foo <em>bar</em> baz</em></p>",example:407,start_line:6855,end_line:6859,section:"Emphasis and strong emphasis"},{markdown:`__foo_ bar_
`,html:`<p><em><em>foo</em> bar</em></p>
`,example:408,start_line:6862,end_line:6866,section:"Emphasis and strong emphasis"},{markdown:`*foo *bar**
`,html:`<p><em>foo <em>bar</em></em></p>
`,example:409,start_line:6869,end_line:6873,section:"Emphasis and strong emphasis"},{markdown:`*foo **bar** baz*
`,html:"<p><em>foo <strong>bar</strong> baz</em></p>",example:410,start_line:6876,end_line:6880,section:"Emphasis and strong emphasis"},{markdown:`*foo**bar**baz*
`,html:"<p><em>foo<strong>bar</strong>baz</em></p>",example:411,start_line:6882,end_line:6886,section:"Emphasis and strong emphasis"},{markdown:`*foo**bar*
`,html:"<p><em>foo**bar</em></p>",example:412,start_line:6906,end_line:6910,section:"Emphasis and strong emphasis"},{markdown:`***foo** bar*
`,html:`<p><em><strong>foo</strong> bar</em></p>
`,example:413,start_line:6919,end_line:6923,section:"Emphasis and strong emphasis"},{markdown:`*foo **bar***
`,html:"<p><em>foo <strong>bar</strong></em></p>",example:414,start_line:6926,end_line:6930,section:"Emphasis and strong emphasis"},{markdown:`*foo**bar***
`,html:"<p><em>foo<strong>bar</strong></em></p>",example:415,start_line:6933,end_line:6937,section:"Emphasis and strong emphasis"},{markdown:`foo***bar***baz
`,html:`<p>foo<em><strong>bar</strong></em>baz</p>
`,example:416,start_line:6944,end_line:6948,section:"Emphasis and strong emphasis"},{markdown:`foo******bar*********baz
`,html:`<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>
`,example:417,start_line:6950,end_line:6954,section:"Emphasis and strong emphasis"},{markdown:`*foo **bar *baz* bim** bop*
`,html:"<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>",example:418,start_line:6959,end_line:6963,section:"Emphasis and strong emphasis"},{markdown:`*foo [*bar*](/url)*
`,html:'<p><em>foo <a href="/url"><em>bar</em></a></em></p>',example:419,start_line:6966,end_line:6970,section:"Emphasis and strong emphasis"},{markdown:`** is not an empty emphasis
`,html:"<p>** is not an empty emphasis</p>",example:420,start_line:6975,end_line:6979,section:"Emphasis and strong emphasis"},{markdown:`**** is not an empty strong emphasis
`,html:"<p>**** is not an empty strong emphasis</p>",example:421,start_line:6982,end_line:6986,section:"Emphasis and strong emphasis"},{markdown:`**foo [bar](/url)**
`,html:'<p><strong>foo <a href="/url">bar</a></strong></p>',example:422,start_line:6995,end_line:6999,section:"Emphasis and strong emphasis"},{markdown:`**foo
bar**
`,html:`<p><strong>foo
bar</strong></p>`,example:423,start_line:7002,end_line:7008,section:"Emphasis and strong emphasis"},{markdown:`__foo _bar_ baz__
`,html:"<p><strong>foo <em>bar</em> baz</strong></p>",example:424,start_line:7014,end_line:7018,section:"Emphasis and strong emphasis"},{markdown:`__foo __bar__ baz__
`,html:"<p><strong>foo <strong>bar</strong> baz</strong></p>",example:425,start_line:7021,end_line:7025,section:"Emphasis and strong emphasis"},{markdown:`____foo__ bar__
`,html:`<p><strong><strong>foo</strong> bar</strong></p>
`,example:426,start_line:7028,end_line:7032,section:"Emphasis and strong emphasis"},{markdown:`**foo **bar****
`,html:"<p><strong>foo <strong>bar</strong></strong></p>",example:427,start_line:7035,end_line:7039,section:"Emphasis and strong emphasis"},{markdown:`**foo *bar* baz**
`,html:"<p><strong>foo <em>bar</em> baz</strong></p>",example:428,start_line:7042,end_line:7046,section:"Emphasis and strong emphasis"},{markdown:`**foo*bar*baz**
`,html:"<p><strong>foo<em>bar</em>baz</strong></p>",example:429,start_line:7049,end_line:7053,section:"Emphasis and strong emphasis"},{markdown:`***foo* bar**
`,html:`<p><strong><em>foo</em> bar</strong></p>
`,example:430,start_line:7056,end_line:7060,section:"Emphasis and strong emphasis"},{markdown:`**foo *bar***
`,html:"<p><strong>foo <em>bar</em></strong></p>",example:431,start_line:7063,end_line:7067,section:"Emphasis and strong emphasis"},{markdown:`**foo *bar **baz**
bim* bop**
`,html:`<p><strong>foo <em>bar <strong>baz</strong>
bim</em> bop</strong></p>`,example:432,start_line:7072,end_line:7078,section:"Emphasis and strong emphasis"},{markdown:`**foo [*bar*](/url)**
`,html:'<p><strong>foo <a href="/url"><em>bar</em></a></strong></p>',example:433,start_line:7081,end_line:7085,section:"Emphasis and strong emphasis"},{markdown:`__ is not an empty emphasis
`,html:"<p>__ is not an empty emphasis</p>",example:434,start_line:7090,end_line:7094,section:"Emphasis and strong emphasis"},{markdown:`____ is not an empty strong emphasis
`,html:"<p>____ is not an empty strong emphasis</p>",example:435,start_line:7097,end_line:7101,section:"Emphasis and strong emphasis"},{markdown:`foo ***
`,html:"<p>foo ***</p>",example:436,start_line:7107,end_line:7111,section:"Emphasis and strong emphasis"},{markdown:`foo *\\**
`,html:"<p>foo <em>*</em></p>",example:437,start_line:7114,end_line:7118,section:"Emphasis and strong emphasis"},{markdown:`foo *_*
`,html:"<p>foo <em>_</em></p>",example:438,start_line:7121,end_line:7125,section:"Emphasis and strong emphasis"},{markdown:`foo *****
`,html:"<p>foo *****</p>",example:439,start_line:7128,end_line:7132,section:"Emphasis and strong emphasis"},{markdown:`foo **\\***
`,html:"<p>foo <strong>*</strong></p>",example:440,start_line:7135,end_line:7139,section:"Emphasis and strong emphasis"},{markdown:`foo **_**
`,html:"<p>foo <strong>_</strong></p>",example:441,start_line:7142,end_line:7146,section:"Emphasis and strong emphasis"},{markdown:`**foo*
`,html:"<p>*<em>foo</em></p>",example:442,start_line:7153,end_line:7157,section:"Emphasis and strong emphasis"},{markdown:`*foo**
`,html:`<p><em>foo</em>*</p>
`,example:443,start_line:7160,end_line:7164,section:"Emphasis and strong emphasis"},{markdown:`***foo**
`,html:"<p>*<strong>foo</strong></p>",example:444,start_line:7167,end_line:7171,section:"Emphasis and strong emphasis"},{markdown:`****foo*
`,html:"<p>***<em>foo</em></p>",example:445,start_line:7174,end_line:7178,section:"Emphasis and strong emphasis"},{markdown:`**foo***
`,html:`<p><strong>foo</strong>*</p>
`,example:446,start_line:7181,end_line:7185,section:"Emphasis and strong emphasis"},{markdown:`*foo****
`,html:`<p><em>foo</em>***</p>
`,example:447,start_line:7188,end_line:7192,section:"Emphasis and strong emphasis"},{markdown:`foo ___
`,html:"<p>foo ___</p>",example:448,start_line:7198,end_line:7202,section:"Emphasis and strong emphasis"},{markdown:`foo _\\__
`,html:"<p>foo <em>_</em></p>",example:449,start_line:7205,end_line:7209,section:"Emphasis and strong emphasis"},{markdown:`foo _*_
`,html:"<p>foo <em>*</em></p>",example:450,start_line:7212,end_line:7216,section:"Emphasis and strong emphasis"},{markdown:`foo _____
`,html:"<p>foo _____</p>",example:451,start_line:7219,end_line:7223,section:"Emphasis and strong emphasis"},{markdown:`foo __\\___
`,html:"<p>foo <strong>_</strong></p>",example:452,start_line:7226,end_line:7230,section:"Emphasis and strong emphasis"},{markdown:`foo __*__
`,html:"<p>foo <strong>*</strong></p>",example:453,start_line:7233,end_line:7237,section:"Emphasis and strong emphasis"},{markdown:`__foo_
`,html:"<p>_<em>foo</em></p>",example:454,start_line:7240,end_line:7244,section:"Emphasis and strong emphasis"},{markdown:`_foo__
`,html:`<p><em>foo</em>_</p>
`,example:455,start_line:7251,end_line:7255,section:"Emphasis and strong emphasis"},{markdown:`___foo__
`,html:"<p>_<strong>foo</strong></p>",example:456,start_line:7258,end_line:7262,section:"Emphasis and strong emphasis"},{markdown:`____foo_
`,html:"<p>___<em>foo</em></p>",example:457,start_line:7265,end_line:7269,section:"Emphasis and strong emphasis"},{markdown:`__foo___
`,html:`<p><strong>foo</strong>_</p>
`,example:458,start_line:7272,end_line:7276,section:"Emphasis and strong emphasis"},{markdown:`_foo____
`,html:`<p><em>foo</em>___</p>
`,example:459,start_line:7279,end_line:7283,section:"Emphasis and strong emphasis"},{markdown:`**foo**
`,html:"<p><strong>foo</strong></p>",example:460,start_line:7289,end_line:7293,section:"Emphasis and strong emphasis"},{markdown:`*_foo_*
`,html:"<p><em><em>foo</em></em></p>",example:461,start_line:7296,end_line:7300,section:"Emphasis and strong emphasis"},{markdown:`__foo__
`,html:"<p><strong>foo</strong></p>",example:462,start_line:7303,end_line:7307,section:"Emphasis and strong emphasis"},{markdown:`_*foo*_
`,html:"<p><em><em>foo</em></em></p>",example:463,start_line:7310,end_line:7314,section:"Emphasis and strong emphasis"},{markdown:`****foo****
`,html:`<p><strong><strong>foo</strong></strong></p>
`,example:464,start_line:7320,end_line:7324,section:"Emphasis and strong emphasis"},{markdown:`____foo____
`,html:`<p><strong><strong>foo</strong></strong></p>
`,example:465,start_line:7327,end_line:7331,section:"Emphasis and strong emphasis"},{markdown:`******foo******
`,html:`<p><strong><strong><strong>foo</strong></strong></strong></p>
`,example:466,start_line:7338,end_line:7342,section:"Emphasis and strong emphasis"},{markdown:`***foo***
`,html:`<p><em><strong>foo</strong></em></p>
`,example:467,start_line:7347,end_line:7351,section:"Emphasis and strong emphasis"},{markdown:`_____foo_____
`,html:`<p><em><strong><strong>foo</strong></strong></em></p>
`,example:468,start_line:7354,end_line:7358,section:"Emphasis and strong emphasis"},{markdown:`*foo _bar* baz_
`,html:"<p><em>foo _bar</em> baz_</p>",example:469,start_line:7363,end_line:7367,section:"Emphasis and strong emphasis"},{markdown:`*foo __bar *baz bim__ bam*
`,html:"<p><em>foo <strong>bar *baz bim</strong> bam</em></p>",example:470,start_line:7370,end_line:7374,section:"Emphasis and strong emphasis"},{markdown:`**foo **bar baz**
`,html:`<p>**foo <strong>bar baz</strong></p>
`,example:471,start_line:7379,end_line:7383,section:"Emphasis and strong emphasis"},{markdown:`*foo *bar baz*
`,html:`<p>*foo <em>bar baz</em></p>
`,example:472,start_line:7386,end_line:7390,section:"Emphasis and strong emphasis"},{markdown:`*[bar*](/url)
`,html:`<p>*<a href="/url">bar*</a></p>
`,example:473,start_line:7395,end_line:7399,section:"Emphasis and strong emphasis"},{markdown:`_foo [bar_](/url)
`,html:`<p>_foo <a href="/url">bar_</a></p>
`,example:474,start_line:7402,end_line:7406,section:"Emphasis and strong emphasis"},{markdown:`*<img src="foo" title="*"/>
`,html:'<p>*<img src="foo" title="*"></p>',example:475,start_line:7409,end_line:7413,section:"Emphasis and strong emphasis"},{markdown:`**<a href="**">
`,html:'<p>**<a href="**"></a></p>',example:476,start_line:7416,end_line:7420,section:"Emphasis and strong emphasis"},{markdown:`__<a href="__">
`,html:'<p>__<a href="__"></a></p>',example:477,start_line:7423,end_line:7427,section:"Emphasis and strong emphasis"},{markdown:"*a `*`*\n",html:`<p><em>a <code>*</code></em></p>
`,example:478,start_line:7430,end_line:7434,section:"Emphasis and strong emphasis"},{markdown:"_a `_`_\n",html:`<p><em>a <code>_</code></em></p>
`,example:479,start_line:7437,end_line:7441,section:"Emphasis and strong emphasis"},{markdown:`**a<https://foo.bar/?q=**>
`,html:`<p>**a<a href="https://foo.bar/?q=**">https://foo.bar/?q=**</a></p>
`,example:480,start_line:7444,end_line:7448,section:"Emphasis and strong emphasis"},{markdown:`__a<https://foo.bar/?q=__>
`,html:`<p>__a<a href="https://foo.bar/?q=__">https://foo.bar/?q=__</a></p>
`,example:481,start_line:7451,end_line:7455,section:"Emphasis and strong emphasis"},{markdown:`[link](/uri "title")
`,html:'<p><a href="/uri" title="title">link</a></p>',example:482,start_line:7539,end_line:7543,section:"Links"},{markdown:`[link](/uri)
`,html:'<p><a href="/uri">link</a></p>',example:483,start_line:7549,end_line:7553,section:"Links"},{markdown:`[](./target.md)
`,html:'<p><a href="./target.md"></a></p>',example:484,start_line:7555,end_line:7559,section:"Links"},{markdown:`[link]()
`,html:"<p><a>link</a></p>",example:485,start_line:7562,end_line:7566,section:"Links"},{markdown:`[link](<>)
`,html:"<p><a>link</a></p>",example:486,start_line:7569,end_line:7573,section:"Links"},{markdown:`[]()
`,html:"<p><a></a></p>",example:487,start_line:7576,end_line:7580,section:"Links"},{markdown:`[link](/my uri)
`,html:"<p>[link](/my uri)</p>",example:488,start_line:7585,end_line:7589,section:"Links"},{markdown:`[link](</my uri>)
`,html:'<p><a href="/my uri">link</a></p>',example:489,start_line:7591,end_line:7595,section:"Links"},{markdown:`[link](foo
bar)
`,html:`<p>[link](foo
bar)</p>`,example:490,start_line:7600,end_line:7606,section:"Links"},{markdown:`[link](<foo
bar>)
`,html:'<p>[link](<foo bar=""></foo>)</p>',example:491,start_line:7608,end_line:7614,section:"Links"},{markdown:`[a](<b)c>)
`,html:'<p><a href="b)c">a</a></p>',example:492,start_line:7619,end_line:7623,section:"Links"},{markdown:`[link](<foo\\>)
`,html:"<p>[link](&lt;foo&gt;)</p>",example:493,start_line:7627,end_line:7631,section:"Links"},{markdown:`[a](<b)c
[a](<b)c>
[a](<b>c)
`,html:`<p>[a](&lt;b)c
[a](&lt;b)c&gt;
[a](<b></b>c)</p>`,example:494,start_line:7636,end_line:7644,section:"Links"},{markdown:`[link](\\(foo\\))
`,html:'<p><a href="(foo)">link</a></p>',example:495,start_line:7648,end_line:7652,section:"Links"},{markdown:`[link](foo(and(bar)))
`,html:`<p><a href="foo(and(bar))">link</a></p>
`,example:496,start_line:7657,end_line:7661,section:"Links"},{markdown:`[link](foo(and(bar))
`,html:"<p>[link](foo(and(bar))</p>",example:497,start_line:7666,end_line:7670,section:"Links"},{markdown:`[link](foo\\(and\\(bar\\))
`,html:'<p><a href="foo(and(bar)">link</a></p>',example:498,start_line:7673,end_line:7677,section:"Links"},{markdown:`[link](<foo(and(bar)>)
`,html:'<p><a href="foo(and(bar)">link</a></p>',example:499,start_line:7680,end_line:7684,section:"Links"},{markdown:`[link](foo\\)\\:)
`,html:'<p><a href="foo):">link</a></p>',example:500,start_line:7690,end_line:7694,section:"Links"},{markdown:`[link](#fragment)

[link](https://example.com#fragment)

[link](https://example.com?foo=3#frag)
`,html:'<p><a href="#fragment">link</a></p><p><a href="https://example.com#fragment">link</a></p><p><a href="https://example.com?foo=3#frag">link</a></p>',example:501,start_line:7699,end_line:7709,section:"Links"},{markdown:`[link](foo\\bar)
`,html:'<p><a href="foo\\bar">link</a></p>',example:502,start_line:7715,end_line:7719,section:"Links"},{markdown:`[link](foo%20b&auml;)
`,html:'<p><a href="foo%20b&amp;auml;">link</a></p>',example:503,start_line:7731,end_line:7735,section:"Links"},{markdown:`[link]("title")
`,html:'<p><a href="&quot;title&quot;">link</a></p>',example:504,start_line:7742,end_line:7746,section:"Links"},{markdown:`[link](/url "title")
[link](/url 'title')
[link](/url (title))
`,html:`<p><a href="/url" title="title">link</a>
<a href="/url" title="title">link</a>
<a href="/url" title="title">link</a></p>`,example:505,start_line:7751,end_line:7759,section:"Links"},{markdown:`[link](/url "title \\"&quot;")
`,html:`<p><a href="/url" title="title &quot;&quot;">link</a></p>
`,example:506,start_line:7765,end_line:7769,section:"Links"},{markdown:`[link](/url\xA0"title")
`,html:'<p><a href="/url&nbsp;&quot;title&quot;">link</a></p>',example:507,start_line:7776,end_line:7780,section:"Links"},{markdown:`[link](/url "title "and" title")
`,html:'<p>[link](/url "title "and" title")</p>',example:508,start_line:7785,end_line:7789,section:"Links"},{markdown:`[link](/url 'title "and" title')
`,html:'<p><a href="/url" title="title &quot;and&quot; title">link</a></p>',example:509,start_line:7794,end_line:7798,section:"Links"},{markdown:`[link](   /uri
  "title"  )
`,html:'<p><a href="/uri" title="title">link</a></p>',example:510,start_line:7819,end_line:7824,section:"Links"},{markdown:`[link] (/uri)
`,html:"<p><a>link</a> (/uri)</p>",example:511,start_line:7830,end_line:7834,section:"Links"},{markdown:`[link [foo [bar]]](/uri)
`,html:`<p><a href="/uri">link [foo [bar]]</a></p>
`,example:512,start_line:7840,end_line:7844,section:"Links"},{markdown:`[link] bar](/uri)
`,html:"<p><a>link</a> bar](/uri)</p>",example:513,start_line:7847,end_line:7851,section:"Links"},{markdown:`[link [bar](/uri)
`,html:'<p>[link <a href="/uri">bar</a></p>',example:514,start_line:7854,end_line:7858,section:"Links"},{markdown:`[link \\[bar](/uri)
`,html:'<p><a href="/uri">link [bar</a></p>',example:515,start_line:7861,end_line:7865,section:"Links"},{markdown:"[link *foo **bar** `#`*](/uri)\n",html:'<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>',example:516,start_line:7870,end_line:7874,section:"Links"},{markdown:`[![moon](moon.jpg)](/uri)
`,html:'<p><a href="/uri"><img src="moon.jpg" alt="moon"></a></p>',example:517,start_line:7877,end_line:7881,section:"Links"},{markdown:`[foo [bar](/uri)](/uri)
`,html:'<p>[foo <a href="/uri">bar</a>](/uri)</p>',example:518,start_line:7886,end_line:7890,section:"Links"},{markdown:`[foo *[bar [baz](/uri)](/uri)*](/uri)
`,html:'<p>[foo <em>[bar <a href="/uri">baz</a>](/uri)</em>](/uri)</p>',example:519,start_line:7893,end_line:7897,section:"Links"},{markdown:`![[[foo](uri1)](uri2)](uri3)
`,html:`<p><img src="uri3" alt="[foo](uri2)" /></p>
`,example:520,start_line:7900,end_line:7904,section:"Links"},{markdown:`*[foo*](/uri)
`,html:`<p>*<a href="/uri">foo*</a></p>
`,example:521,start_line:7910,end_line:7914,section:"Links"},{markdown:`[foo *bar](baz*)
`,html:'<p><a href="baz*">foo *bar</a></p>',example:522,start_line:7917,end_line:7921,section:"Links"},{markdown:`*foo [bar* baz]
`,html:"<p><em>foo [bar</em> baz]</p>",example:523,start_line:7927,end_line:7931,section:"Links"},{markdown:`[foo <bar attr="](baz)">
`,html:`<p>[foo <bar attr="](baz)"></p>
`,example:524,start_line:7937,end_line:7941,section:"Links"},{markdown:"[foo`](/uri)`\n",html:`<p>[foo<code>](/uri)</code></p>
`,example:525,start_line:7944,end_line:7948,section:"Links"},{markdown:`[foo<https://example.com/?search=](uri)>
`,html:`<p>[foo<a href="https://example.com/?search=%5D(uri)">https://example.com/?search=](uri)</a></p>
`,example:526,start_line:7951,end_line:7955,section:"Links"},{markdown:`[foo][bar]

[bar]: /url "title"
`,html:'<p><a href="/url" title="title">foo</a></p>',example:527,start_line:7989,end_line:7995,section:"Links"},{markdown:`[link [foo [bar]]][ref]

[ref]: /uri
`,html:`<p><a href="/uri">link [foo [bar]]</a></p>
`,example:528,start_line:8004,end_line:8010,section:"Links"},{markdown:`[link \\[bar][ref]

[ref]: /uri
`,html:'<p><a href="/uri">link [bar</a></p>',example:529,start_line:8013,end_line:8019,section:"Links"},{markdown:`[link *foo **bar** \`#\`*][ref]

[ref]: /uri
`,html:'<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>',example:530,start_line:8024,end_line:8030,section:"Links"},{markdown:`[![moon](moon.jpg)][ref]

[ref]: /uri
`,html:'<p><a href="/uri"><img src="moon.jpg" alt="moon"></a></p>',example:531,start_line:8033,end_line:8039,section:"Links"},{markdown:`[foo [bar](/uri)][ref]

[ref]: /uri
`,html:'<p>[foo <a href="/uri">bar</a>]<a href="/uri">ref</a></p>',example:532,start_line:8044,end_line:8050,section:"Links"},{markdown:`[foo *bar [baz][ref]*][ref]

[ref]: /uri
`,html:'<p>[foo <em>bar <a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>',example:533,start_line:8053,end_line:8059,section:"Links"},{markdown:`*[foo*][ref]

[ref]: /uri
`,html:`<p>*<a href="/uri">foo*</a></p>
`,example:534,start_line:8068,end_line:8074,section:"Links"},{markdown:`[foo *bar][ref]*

[ref]: /uri
`,html:'<p><a href="/uri">foo *bar</a>*</p>',example:535,start_line:8077,end_line:8083,section:"Links"},{markdown:`[foo <bar attr="][ref]">

[ref]: /uri
`,html:`<p>[foo <bar attr="][ref]"></p>
`,example:536,start_line:8089,end_line:8095,section:"Links"},{markdown:`[foo\`][ref]\`

[ref]: /uri
`,html:`<p>[foo<code>][ref]</code></p>
`,example:537,start_line:8098,end_line:8104,section:"Links"},{markdown:`[foo<https://example.com/?search=][ref]>

[ref]: /uri
`,html:`<p>[foo<a href="https://example.com/?search=%5D%5Bref%5D">https://example.com/?search=][ref]</a></p>
`,example:538,start_line:8107,end_line:8113,section:"Links"},{markdown:`[foo][BaR]

[bar]: /url "title"
`,html:'<p><a href="/url" title="title">foo</a></p>',example:539,start_line:8118,end_line:8124,section:"Links"},{markdown:`[\u1E9E]

[SS]: /url
`,html:`<p><a href="/url">\u1E9E</a></p>
`,example:540,start_line:8129,end_line:8135,section:"Links"},{markdown:`[Foo
  bar]: /url

[Baz][Foo bar]
`,html:`<p><a href="/url">Baz</a></p>
`,example:541,start_line:8141,end_line:8148,section:"Links"},{markdown:`[foo] [bar]

[bar]: /url "title"
`,html:'<p><a>foo</a> <a href="/url" title="title">bar</a></p>',example:542,start_line:8154,end_line:8160,section:"Links"},{markdown:`[foo]
[bar]

[bar]: /url "title"
`,html:`<p><a>foo</a>
<a href="/url" title="title">bar</a></p>`,example:543,start_line:8163,end_line:8171,section:"Links"},{markdown:`[foo]: /url1

[foo]: /url2

[bar][foo]
`,html:'<p><a href="/url1">bar</a></p>',example:544,start_line:8204,end_line:8212,section:"Links"},{markdown:`[bar][foo\\!]

[foo!]: /url
`,html:`<p>[bar][foo!]</p>
`,example:545,start_line:8219,end_line:8225,section:"Links"},{markdown:`[foo][ref[]

[ref[]: /uri
`,html:"<p><a>foo</a>[ref<a></a></p><p>[ref[]: /uri</p>",example:546,start_line:8231,end_line:8238,section:"Links"},{markdown:`[foo][ref[bar]]

[ref[bar]]: /uri
`,html:"<p><a>foo</a>[ref<a>bar</a>]</p><p>[ref<a>bar</a>]: /uri</p>",example:547,start_line:8241,end_line:8248,section:"Links"},{markdown:`[[[foo]]]

[[[foo]]]: /url
`,html:"<p>[[<a>foo</a>]]</p><p>[[<a>foo</a>]]: /url</p>",example:548,start_line:8251,end_line:8258,section:"Links"},{markdown:`[foo][ref\\[]

[ref\\[]: /uri
`,html:'<p><a href="/uri">foo</a></p>',example:549,start_line:8261,end_line:8267,section:"Links"},{markdown:`[bar\\\\]: /uri

[bar\\\\]
`,html:'<p><a href="/uri">bar\\</a></p>',example:550,start_line:8272,end_line:8278,section:"Links"},{markdown:`[]

[]: /uri
`,html:"<p><a></a></p><p>[]: /uri</p>",example:551,start_line:8284,end_line:8291,section:"Links"},{markdown:`[
 ]

[
 ]: /uri
`,html:`<p><a></a></p><p>[
]: /uri</p>`,example:552,start_line:8294,end_line:8305,section:"Links"},{markdown:`[foo][]

[foo]: /url "title"
`,html:'<p><a href="/url" title="title">foo</a></p>',example:553,start_line:8317,end_line:8323,section:"Links"},{markdown:`[*foo* bar][]

[*foo* bar]: /url "title"
`,html:'<p><a href="/url" title="title"><em>foo</em> bar</a></p>',example:554,start_line:8326,end_line:8332,section:"Links"},{markdown:`[Foo][]

[foo]: /url "title"
`,html:'<p><a href="/url" title="title">Foo</a></p>',example:555,start_line:8337,end_line:8343,section:"Links"},{markdown:`[foo] 
[]

[foo]: /url "title"
`,html:`<p><a href="/url" title="title">foo</a>
<a></a></p>`,example:556,start_line:8350,end_line:8358,section:"Links"},{markdown:`[foo]

[foo]: /url "title"
`,html:'<p><a href="/url" title="title">foo</a></p>',example:557,start_line:8370,end_line:8376,section:"Links"},{markdown:`[*foo* bar]

[*foo* bar]: /url "title"
`,html:'<p><a href="/url" title="title"><em>foo</em> bar</a></p>',example:558,start_line:8379,end_line:8385,section:"Links"},{markdown:`[[*foo* bar]]

[*foo* bar]: /url "title"
`,html:'<p>[<a href="/url" title="title"><em>foo</em> bar</a>]</p>',example:559,start_line:8388,end_line:8394,section:"Links"},{markdown:`[[bar [foo]

[foo]: /url
`,html:'<p>[[bar <a href="/url">foo</a></p>',example:560,start_line:8397,end_line:8403,section:"Links"},{markdown:`[Foo]

[foo]: /url "title"
`,html:'<p><a href="/url" title="title">Foo</a></p>',example:561,start_line:8408,end_line:8414,section:"Links"},{markdown:`[foo] bar

[foo]: /url
`,html:'<p><a href="/url">foo</a> bar</p>',example:562,start_line:8419,end_line:8425,section:"Links"},{markdown:`\\[foo]

[foo]: /url "title"
`,html:"<p>[foo]</p>",example:563,start_line:8431,end_line:8437,section:"Links"},{markdown:`[foo*]: /url

*[foo*]
`,html:`<p>*<a href="/url">foo*</a></p>
`,example:564,start_line:8443,end_line:8449,section:"Links"},{markdown:`[foo][bar]

[foo]: /url1
[bar]: /url2
`,html:'<p><a href="/url2">foo</a></p>',example:565,start_line:8455,end_line:8462,section:"Links"},{markdown:`[foo][]

[foo]: /url1
`,html:'<p><a href="/url1">foo</a></p>',example:566,start_line:8464,end_line:8470,section:"Links"},{markdown:`[foo]()

[foo]: /url1
`,html:"<p><a>foo</a></p>",example:567,start_line:8474,end_line:8480,section:"Links"},{markdown:`[foo](not a link)

[foo]: /url1
`,html:`<p><a href="/url1">foo</a>(not a link)</p>
`,example:568,start_line:8482,end_line:8488,section:"Links"},{markdown:`[foo][bar][baz]

[baz]: /url
`,html:`<p>[foo]<a href="/url">bar</a></p>
`,example:569,start_line:8493,end_line:8499,section:"Links"},{markdown:`[foo][bar][baz]

[baz]: /url1
[bar]: /url2
`,html:'<p><a href="/url2">foo</a><a href="/url1">baz</a></p>',example:570,start_line:8505,end_line:8512,section:"Links"},{markdown:`[foo][bar][baz]

[baz]: /url1
[foo]: /url2
`,html:`<p>[foo]<a href="/url1">bar</a></p>
`,example:571,start_line:8518,end_line:8525,section:"Links"},{markdown:`![foo](/url "title")
`,html:'<p><img src="/url" alt="foo" title="title"></p>',example:572,start_line:8541,end_line:8545,section:"Images"},{markdown:`![foo *bar*]

[foo *bar*]: train.jpg "train & tracks"
`,html:'<p><img alt="foo *bar*" src="train.jpg" title="train &amp; tracks"></p>',example:573,start_line:8548,end_line:8554,section:"Images"},{markdown:`![foo ![bar](/url)](/url2)
`,html:`<p><img src="/url2" alt="foo bar" /></p>
`,example:574,start_line:8557,end_line:8561,section:"Images"},{markdown:`![foo [bar](/url)](/url2)
`,html:'<p><img src="/url2" alt="foo [bar](/url)"></p>',example:575,start_line:8564,end_line:8568,section:"Images"},{markdown:`![foo *bar*][]

[foo *bar*]: train.jpg "train & tracks"
`,html:'<p><img alt="foo *bar*" src="train.jpg" title="train &amp; tracks"></p>',example:576,start_line:8578,end_line:8584,section:"Images"},{markdown:`![foo *bar*][foobar]

[FOOBAR]: train.jpg "train & tracks"
`,html:'<p><img alt="foo *bar*" src="train.jpg" title="train &amp; tracks"></p>',example:577,start_line:8587,end_line:8593,section:"Images"},{markdown:`![foo](train.jpg)
`,html:'<p><img src="train.jpg" alt="foo"></p>',example:578,start_line:8596,end_line:8600,section:"Images"},{markdown:`My ![foo bar](/path/to/train.jpg  "title"   )
`,html:'<p>My <img src="/path/to/train.jpg" alt="foo bar" title="title"></p>',example:579,start_line:8603,end_line:8607,section:"Images"},{markdown:`![foo](<url>)
`,html:'<p><img src="url" alt="foo"></p>',example:580,start_line:8610,end_line:8614,section:"Images"},{markdown:`![](/url)
`,html:'<p><img src="/url" alt=""></p>',example:581,start_line:8617,end_line:8621,section:"Images"},{markdown:`![foo][bar]

[bar]: /url
`,html:'<p><img alt="foo" src="/url"></p>',example:582,start_line:8626,end_line:8632,section:"Images"},{markdown:`![foo][bar]

[BAR]: /url
`,html:'<p><img alt="foo" src="/url"></p>',example:583,start_line:8635,end_line:8641,section:"Images"},{markdown:`![foo][]

[foo]: /url "title"
`,html:'<p><img alt="foo" src="/url" title="title"></p>',example:584,start_line:8646,end_line:8652,section:"Images"},{markdown:`![*foo* bar][]

[*foo* bar]: /url "title"
`,html:'<p><img alt="*foo* bar" src="/url" title="title"></p>',example:585,start_line:8655,end_line:8661,section:"Images"},{markdown:`![Foo][]

[foo]: /url "title"
`,html:'<p><img alt="Foo" src="/url" title="title"></p>',example:586,start_line:8666,end_line:8672,section:"Images"},{markdown:`![foo] 
[]

[foo]: /url "title"
`,html:`<p><img alt="foo" src="/url" title="title">
<a></a></p>`,example:587,start_line:8678,end_line:8686,section:"Images"},{markdown:`![foo]

[foo]: /url "title"
`,html:'<p><img alt="foo" src="/url" title="title"></p>',example:588,start_line:8691,end_line:8697,section:"Images"},{markdown:`![*foo* bar]

[*foo* bar]: /url "title"
`,html:'<p><img alt="*foo* bar" src="/url" title="title"></p>',example:589,start_line:8700,end_line:8706,section:"Images"},{markdown:`![[foo]]

[[foo]]: /url "title"
`,html:`<p>![[foo]]</p>
<p>[[foo]]: /url &quot;title&quot;</p>
`,example:590,start_line:8711,end_line:8718,section:"Images"},{markdown:`![Foo]

[foo]: /url "title"
`,html:'<p><img alt="Foo" src="/url" title="title"></p>',example:591,start_line:8723,end_line:8729,section:"Images"},{markdown:`!\\[foo]

[foo]: /url "title"
`,html:"<p>![foo]</p>",example:592,start_line:8735,end_line:8741,section:"Images"},{markdown:`\\![foo]

[foo]: /url "title"
`,html:'<p>!<a href="/url" title="title">foo</a></p>',example:593,start_line:8747,end_line:8753,section:"Images"},{markdown:`<http://foo.bar.baz>
`,html:'<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>',example:594,start_line:8780,end_line:8784,section:"Autolinks"},{markdown:`<https://foo.bar.baz/test?q=hello&id=22&boolean>
`,html:'<p><a href="https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>',example:595,start_line:8787,end_line:8791,section:"Autolinks"},{markdown:`<irc://foo.bar:2233/baz>
`,html:'<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>',example:596,start_line:8794,end_line:8798,section:"Autolinks"},{markdown:`<MAILTO:FOO@BAR.BAZ>
`,html:'<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>',example:597,start_line:8803,end_line:8807,section:"Autolinks"},{markdown:`<a+b+c:d>
`,html:'<p><a href="a+b+c:d">a+b+c:d</a></p>',example:598,start_line:8815,end_line:8819,section:"Autolinks"},{markdown:`<made-up-scheme://foo,bar>
`,html:'<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>',example:599,start_line:8822,end_line:8826,section:"Autolinks"},{markdown:`<https://../>
`,html:'<p><a href="https://../">https://../</a></p>',example:600,start_line:8829,end_line:8833,section:"Autolinks"},{markdown:`<localhost:5001/foo>
`,html:'<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>',example:601,start_line:8836,end_line:8840,section:"Autolinks"},{markdown:`<https://foo.bar/baz bim>
`,html:"<p>&lt;https://foo.bar/baz bim&gt;</p>",example:602,start_line:8845,end_line:8849,section:"Autolinks"},{markdown:`<https://example.com/\\[\\>
`,html:'<p><a href="https://example.com/\\[\\">https://example.com/\\[\\</a></p>',example:603,start_line:8854,end_line:8858,section:"Autolinks"},{markdown:`<foo@bar.example.com>
`,html:'<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>',example:604,start_line:8876,end_line:8880,section:"Autolinks"},{markdown:`<foo+special@Bar.baz-bar0.com>
`,html:'<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>',example:605,start_line:8883,end_line:8887,section:"Autolinks"},{markdown:`<foo\\+@bar.example.com>
`,html:"<p>&lt;foo+@bar.example.com&gt;</p>",example:606,start_line:8892,end_line:8896,section:"Autolinks"},{markdown:`<>
`,html:"<p>&lt;&gt;</p>",example:607,start_line:8901,end_line:8905,section:"Autolinks"},{markdown:`< https://foo.bar >
`,html:"<p>&lt; https://foo.bar &gt;</p>",example:608,start_line:8908,end_line:8912,section:"Autolinks"},{markdown:`<m:abc>
`,html:"<p>&lt;m:abc&gt;</p>",example:609,start_line:8915,end_line:8919,section:"Autolinks"},{markdown:`<foo.bar.baz>
`,html:"<p>&lt;foo.bar.baz&gt;</p>",example:610,start_line:8922,end_line:8926,section:"Autolinks"},{markdown:`https://example.com
`,html:"<p>https://example.com</p>",example:611,start_line:8929,end_line:8933,section:"Autolinks"},{markdown:`foo@bar.example.com
`,html:"<p>foo@bar.example.com</p>",example:612,start_line:8936,end_line:8940,section:"Autolinks"},{markdown:`<a><bab><c2c>
`,html:"<p><a></a><bab></bab><c2c></c2c></p>",example:613,start_line:9016,end_line:9020,section:"Raw HTML"},{markdown:`<a/><b2/>
`,html:"<p><a></a><b2></b2></p>",example:614,start_line:9025,end_line:9029,section:"Raw HTML"},{markdown:`<a  /><b2
data="foo" >
`,html:'<p><a></a><b2 data="foo"></b2></p>',example:615,start_line:9034,end_line:9040,section:"Raw HTML"},{markdown:`<a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 />
`,html:'<p><a foo="bar" bam="baz <em>&quot;</em>" _boolean="" zoop:33="zoop:33"></a></p>',example:616,start_line:9045,end_line:9051,section:"Raw HTML"},{markdown:`Foo <responsive-image src="foo.jpg" />
`,html:'<p>Foo <responsive-image src="foo.jpg"></responsive-image></p>',example:617,start_line:9056,end_line:9060,section:"Raw HTML"},{markdown:`<33> <__>
`,html:"<p>&lt;33&gt; &lt;__&gt;</p>",example:618,start_line:9065,end_line:9069,section:"Raw HTML"},{markdown:`<a h*#ref="hi">
`,html:'<p>&lt;a h*#ref="hi"&gt;</p>',example:619,start_line:9074,end_line:9078,section:"Raw HTML"},{markdown:`<a href="hi'> <a href=hi'>
`,html:`<p>&lt;a href="hi'&gt; &lt;a href=hi'&gt;</p>`,example:620,start_line:9083,end_line:9087,section:"Raw HTML"},{markdown:`< a><
foo><bar/ >
<foo bar=baz
bim!bop />
`,html:`<p>&lt; a&gt;&lt;
foo&gt;&lt;bar/ &gt;
&lt;foo bar=baz
bim!bop /&gt;</p>`,example:621,start_line:9092,end_line:9102,section:"Raw HTML"},{markdown:`<a href='bar'title=title>
`,html:"<p>&lt;a href='bar'title=title&gt;</p>",example:622,start_line:9107,end_line:9111,section:"Raw HTML"},{markdown:`</a></foo >
`,html:"<p>&lt;/a&gt;&lt;/foo &gt;</p>",example:623,start_line:9116,end_line:9120,section:"Raw HTML"},{markdown:`</a href="foo">
`,html:'<p>&lt;/a href="foo"&gt;</p>',example:624,start_line:9125,end_line:9129,section:"Raw HTML"},{markdown:`foo <!-- this is a --
comment - with hyphens -->
`,html:`<p>foo <!-- this is a --
comment - with hyphens --></p>
`,example:625,start_line:9134,end_line:9140,section:"Raw HTML"},{markdown:`foo <!--> foo -->

foo <!---> foo -->
`,html:`<p>foo <!--> foo --&gt;</p>
<p>foo <!---> foo --&gt;</p>
`,example:626,start_line:9142,end_line:9149,section:"Raw HTML"},{markdown:`foo <?php echo $a; ?>
`,html:`<p>foo <?php echo $a; ?></p>
`,example:627,start_line:9154,end_line:9158,section:"Raw HTML"},{markdown:`foo <!ELEMENT br EMPTY>
`,html:`<p>foo <!ELEMENT br EMPTY></p>
`,example:628,start_line:9163,end_line:9167,section:"Raw HTML"},{markdown:`foo <![CDATA[>&<]]>
`,html:`<p>foo <![CDATA[>&<]]></p>
`,example:629,start_line:9172,end_line:9176,section:"Raw HTML"},{markdown:`foo <a href="&ouml;">
`,html:'<p>foo <a href="\xF6"></a></p>',example:630,start_line:9182,end_line:9186,section:"Raw HTML"},{markdown:`foo <a href="\\*">
`,html:'<p>foo <a href="\\*"></a></p>',example:631,start_line:9191,end_line:9195,section:"Raw HTML"},{markdown:`<a href="\\"">
`,html:'<p>&lt;a href="""&gt;</p>',example:632,start_line:9198,end_line:9202,section:"Raw HTML"},{markdown:`foo  
baz
`,html:`<p>foo<br />
baz</p>
`,example:633,start_line:9212,end_line:9218,section:"Hard line breaks"},{markdown:`foo\\
baz
`,html:`<p>foo<br>
baz</p>`,example:634,start_line:9224,end_line:9230,section:"Hard line breaks"},{markdown:`foo       
baz
`,html:`<p>foo<br />
baz</p>
`,example:635,start_line:9235,end_line:9241,section:"Hard line breaks"},{markdown:`foo  
     bar
`,html:`<p>foo<br />
bar</p>
`,example:636,start_line:9246,end_line:9252,section:"Hard line breaks"},{markdown:`foo\\
     bar
`,html:`<p>foo<br>
bar</p>`,example:637,start_line:9255,end_line:9261,section:"Hard line breaks"},{markdown:`*foo  
bar*
`,html:`<p><em>foo<br />
bar</em></p>
`,example:638,start_line:9267,end_line:9273,section:"Hard line breaks"},{markdown:`*foo\\
bar*
`,html:`<p><em>foo<br>
bar</em></p>`,example:639,start_line:9276,end_line:9282,section:"Hard line breaks"},{markdown:"`code  \nspan`\n",html:`<p><code>code   span</code></p>
`,example:640,start_line:9287,end_line:9292,section:"Hard line breaks"},{markdown:"`code\\\nspan`\n",html:"<p><code>code\\ span</code></p>",example:641,start_line:9295,end_line:9300,section:"Hard line breaks"},{markdown:`<a href="foo  
bar">
`,html:`<p><a href="foo  
bar"></p>
`,example:642,start_line:9305,end_line:9311,section:"Hard line breaks"},{markdown:`<a href="foo\\
bar">
`,html:`<p><a href="foo\\
bar"></a></p>`,example:643,start_line:9314,end_line:9320,section:"Hard line breaks"},{markdown:`foo\\
`,html:"<p>foo\\</p>",example:644,start_line:9327,end_line:9331,section:"Hard line breaks"},{markdown:`foo  
`,html:`<p>foo</p>
`,example:645,start_line:9334,end_line:9338,section:"Hard line breaks"},{markdown:`### foo\\
`,html:"<h3>foo\\</h3>",example:646,start_line:9341,end_line:9345,section:"Hard line breaks"},{markdown:`### foo  
`,html:`<h3>foo</h3>
`,example:647,start_line:9348,end_line:9352,section:"Hard line breaks"},{markdown:`foo
baz
`,html:`<p>foo
baz</p>`,example:648,start_line:9363,end_line:9369,section:"Soft line breaks"},{markdown:`foo 
 baz
`,html:`<p>foo
baz</p>`,example:649,start_line:9375,end_line:9381,section:"Soft line breaks"},{markdown:`hello $.;'there
`,html:"<p>hello $.;'there</p>",example:650,start_line:9395,end_line:9399,section:"Textual content"},{markdown:`Foo \u03C7\u03C1\u1FC6\u03BD
`,html:"<p>Foo \u03C7\u03C1\u1FC6\u03BD</p>",example:651,start_line:9402,end_line:9406,section:"Textual content"},{markdown:`Multiple     spaces
`,html:"<p>Multiple     spaces</p>",example:652,start_line:9411,end_line:9415,section:"Textual content"}]});var Z=J(V());function i(e,...n){let o=document.createElement(e);return n.length>0&&o.append(...n),o}function m(e){return document.createTextNode(e)}function I(e){let n=[],o=/\s/g,t=0,a;for(;a=o.exec(e);){t<a.index&&n.push(m(e.substring(t,a.index))),t=a.index+a[0].length;let l=i("span",m(Te(a[0])));l.className="ws",n.push(l)}return t<e.length&&n.push(m(e.slice(t))),n}function Te(e){let n;switch(e){case"	":return"\u2192";case`
`:return`\u21A9
`;case" ":return"\xB7";default:return e}}function qe(e,n,o,t){if(e.find(l=>l.target==o))throw new Error("Transition between source and target already exist");let a={regexp:new RegExp(n,"yui"),target:o,group:t};return e.push(a),a}var w=class e{states;start;accept;groups;current;constructor(n,o,t){this.states=n,this.start=o,this.accept=t,this.groups={},this.current=this.start}static create(n,o){let t=Array.from({length:n+2},()=>[]);if(o.length!=t.length)throw new Error("Wrong number of arguments for the init function");let a=o(...t),l=t[0],s=t[t.length-1];for(let r=0;r<a.length;++r){let[d,h,F,v]=a[r];qe(d,h,F,v)}if(s.length>0)throw new Error("Accept state cannot have outgoing transitions");return new e(t,l,s)}init(){this.current=this.start,this.groups={}}clone(){let n=structuredClone(this);return new e(n.states,n.start,n.accept)}exec(n,o,t=!1){let a=this.current;if(a==this.accept||!t&&o>=n.length)return[!0,o];for(let l=0;l<a.length;++l){let s=a[l];s.regexp.lastIndex=o;let r=s.regexp.exec(n);if(r){if(s.group){let h=r[0];r.index+h.length>=n.length&&(h+=`
`),this.groups[s.group]?this.groups[s.group]+=h:this.groups[s.group]=h}this.current=s.target;let d=this.exec(n,r.index+r[0].length,t);if(d[0])return d}}return[!1,o]}get accepted(){return this.current==this.accept}static concat(...n){let o=n[0].clone(),t=o;for(let a=1;a<n.length;++a){let l=a==n.length-1?n[a]:n[a].clone(),s=t.incoming(t.accept);for(let r=0;r<s.length;++r)s[r].target=l.start;o.states.pop(),o.states.push(...l.states),t=l}return o.accept=t.accept,o}getRegExp(n){return n.map(o=>`(?:${o.regexp.source})`).join("|")}get startRegExp(){return this.getRegExp(this.start)}incoming(n){let o=[];for(let t=0;t<this.states.length;++t){let a=this.states[t];if(a!=n)for(let l=0;l<a.length;++l){let s=a[l];s.target==n&&o.push(s)}}return o}};function p(e,n){return{matched:e,regexp:n}}function T(e,n,o=0){return{input:n,nextIndex:o,blocks:e.blocks,linkRefs:e.linkRefs,links:e.links}}function c(e,n,o,t=!0,a=n,l,s,r){e.blocks.push({element:n,parent:a,type:o,leaf:t,lines:[],cont:l,closing:s,continuing:r})}function x(e){let o=e.blocks.pop().element,t=f(e)?.parent;t&&t!=o&&t.append(o)}function f(e){return e.blocks[e.blocks.length-1]}function B(e){return f(e).element.tagName=="P"}function A(e){b(e),B(e)&&x(e)}function _(e,...n){f(e).parent.append(...n)}function H(e,n){f(e).parent.insertAdjacentHTML("beforeend",n)}function Fe(e,n){n.lines.push(e.input.slice(e.nextIndex)),e.nextIndex=e.input.length}function me(e,n){for(;n.lines.length>0&&n.lines[n.lines.length-1]=="";)n.lines.pop();for(;n.lines.length>0&&n.lines[0]=="";)n.lines.shift()}function pe(e,n){let o=e.map((t,a)=>`(?<g${a}>${t.regexp})`).join("|");return new RegExp(o,n?"yuis":"guis")}function ye(e,n){return new RegExp(`(?<open>${e})|(?<close>${n})`,"guis")}function L(e,n){n.lastIndex=e.nextIndex;let o=n.exec(e.input);return o&&(e.nextIndex=o.index+o[0].length),o}function de(e,n,o,t){if(o.nextIndex>=o.input.length)return!1;e.lastIndex=o.nextIndex;let a=e.exec(o.input);if(a&&a.groups){let l=n.find((s,r)=>a.groups[`g${r}`]!=null);if(l)return t&&he(o,a.index),o.nextIndex=a.index+a[0].length,l.matched(o,a),!0}return!1}var ce=/\\(?<esc>[!"#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~\n])/guis,Ie=/(?<entity>&(?:[a-z]\w*|#\d{1,7}|#[Xx][\da-f]{1,6});)/.source,Ae=/ {4}| {0,3}\t/yuis,Be=/ {4}| {0,3}\t|\s*$/yuis,Y=/ {0,3}> ?/yuis,E=/(?=\s*\S)/yuis,He=/(?<codedelim>`+)(?<code>\s.+\s|(?:[^`]|(?!\k<codedelim>)`)+)\k<codedelim>/.source,Se=/(?<emdelim>(?:(?<!\\)|(?<=\\\\))(?:\*\*?(?![\s\p{P}\p{S}]|$))|(?:(?<=[\s\p{P}\p{S}]|^)\*\*?(?![\P{P}\*])))(?<em>.+)(?:(?<![\s\p{P}\p{S}\\])\k<emdelim>|(?<![\P{P}\\])\k<emdelim>(?=[\s\p{P}\p{S}])|(?<![\s\\])\k<emdelim>$)/u.source,Me=/(?<emdelim>(?:(?<!\\)|(?<=\\\\))(?:__?(?![\s\p{P}\p{S}]|$))|(?:(?<=[\s\p{P}\p{S}]|^)__?(?![\P{P}_])))(?<em>.+)(?:(?<![\s\p{P}\p{S}\\])\k<emdelim>|(?<![\P{P}\\])\k<emdelim>(?=[\s\p{P}\p{S}])|(?<![\s\\])\k<emdelim>$)/u.source,ve=/\[(?<linklabel>(?:\s*(?:[^\[\]\s]|(?<=\\)(?<!\\\\)[\[\]])+\s*)+)\]/.source,Re=/(?:(?<!\\)<(?<linkdest>(?:[^<>\n]|(?<=\\)[<>])*)(?<!\\)>|(?<linkdest>(?!<)(?:[^\x00-\x1F\x7F ()]|(?<=\\)[()])*))/.source,Pe=/(?:"(?<linktitle>(?:[^"]|(?<=\\)")+)"|'(?<linktitle>(?:[^']|(?<=\\)')+)'|\((?<linktitle>(?:[^()]|(?<=\\)[()])+)\))/.source,$e=/(?<!\\)\[/.source,Ce=/(?<!\\)!\[/.source,De=/(?<!\\)!?\[/.source,Ne=/(?:(?<!\\)|(?<=\\\\))\]/.source,K=ye(De,Ne),Q=new RegExp(`\\(\\s*${Re}(?:\\s+${Pe})?\\s*\\)`,"yuis"),ee=new RegExp(ve,"yuis"),ne=/(?![(:])(?:\[\])?/yuis,je=/<(?<autolink>[a-z][\w\-+.]{1,31}:[^\x00-\x1F\x7F <>]*)>/.source,Oe=/<(?<email>[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>/.source,Xe=/(?<tagstart><(?<tag>[a-z][a-z0-9\-]*)(?:\s+[a-z:_][\w.:\-]*\s*(?:=\s*"[^"]*"|=\s*'[^']*'|=[^\s"'=<>`]+)?)*\s*)(?<tagend>\/?>)(?:(?<innerhtml>.*)<\/\k<tag>\s*>)?/.source,Ze=w.create(1,(e,n,o)=>[[e,/ {0,3}\[/,n],[n,/\s*(?:[^\[\]\s]|(?<=\\)(?<!\\\\)[\[\]])+/,n,"label"],[n,/\s*(?:(?<!\\)|(?<=\\\\))\]:\s*/,o]]),Ue=w.create(0,(e,n)=>[[e,/\s*(?:<(?:[^<>]|(?<=\\)[<>])+(?<!\\)>|(?<!<)(?:[^\x00-\x1F\x7F ()]|(?<=\\)[()])+)(?:\s+|$)/,n,"dest"]]),We=w.create(3,(e,n,o,t,a)=>[[e,/\s*"/,n],[n,/(?:\s*(?:[^"\s]|(?<=\\)")+)+/,n,"title"],[n,/(?<!\\)"\s*$/,a],[e,/\s*'/,o],[o,/(?:\s*(?:[^'\s]|(?<=\\)')+)+/,o,"title"],[o,/(?<!\\)'\s*$/,a],[e,/\s*\(/,t],[t,/(?:\s*(?:[^()]|(?<=\\)[()])+)+/,t,"title"],[t,/(?<!\\)\)\s*$/,a],[e,/(?!["'(])|^/,a]]),u=w.concat(Ze,Ue,We);function he(e,n){if(n==null||n>e.nextIndex){let o=e.input.substring(e.nextIndex,n);o&&_(e,m(o))}}function S(e,n,o,t){o.lastIndex=n;let a=o.exec(e);if(a){let{open:l,close:s}=a.groups;if(n=a.index+a[0].length,l&&l==t){let r=S(e,n,o,t);return r?S(e,r.index+r[0].length,o,t):void 0}else if(s)return a}}function oe(e){return p((n,o)=>{let{emdelim:t,em:a}=o.groups;c(n,i(t.length==1?"em":"strong"),1),q(T(n,a)),x(n)},e)}function $(e,n,o,t){let a=i("a");return o&&(a.href=z(o)),t&&(a.title=z(t)),c(e,a,1),q(T(e,n)),x(e),a}function te(e,n,o){n=n.toUpperCase();let t=e.linkRefs[n];if(t)$(e,o,t.destination,t.title);else{let a=e.links[n]||[];a.push($(e,o)),e.links[n]=a}}function C(e,n,o,t){let a=i("img");return o&&(a.src=z(o)),a.alt=n,t&&(a.title=t),_(e,a),a}function ae(e,n,o){n=n.toUpperCase();let t=e.linkRefs[n];if(t)C(e,o,t.destination,t.title);else{let a=e.links[n]||[];a.push(C(e,o)),e.links[n]=a}}function z(e){return e?.replaceAll(ce,n=>n[1])}var le=[p((e,n)=>{let{esc:o}=n.groups;o==`
`&&_(e,i("br")),_(e,m(o))},ce.source),p((e,n)=>{let{entity:o}=n.groups;H(e,o)},Ie),p((e,n)=>{let{code:o}=n.groups;o=o.replaceAll(`
`," ");let t=/^ (.*[^ ].*) $/.exec(o);t&&(o=t[1]),_(e,i("code",m(o)))},He),p((e,n)=>{let o=S(e.input,e.nextIndex,K,"![");if(o){let t=e.input.substring(e.nextIndex,o.index);switch(e.nextIndex=o.index+o[0].length,e.input[e.nextIndex]){case"(":let a=L(e,Q);if(a){let{linkdest:r,linktitle:d}=a.groups;return $(e,t,r,d)}break;case"[":let l=L(e,ee);if(l){let{linklabel:r}=l.groups;return te(e,r,t)}default:if(L(e,ne))return te(e,t,t)}e.nextIndex=n.index+n[0].length}_(e,m(n[0]))},$e),p((e,n)=>{let o=S(e.input,e.nextIndex,K,"[");if(o){let t=e.input.substring(e.nextIndex,o.index);switch(e.nextIndex=o.index+o[0].length,e.input[e.nextIndex]){case"(":let a=L(e,Q);if(a){let{linkdest:r,linktitle:d}=a.groups;return C(e,t,r,d)}break;case"[":let l=L(e,ee);if(l){let{linklabel:r}=l.groups;return ae(e,r,t)}default:if(L(e,ne))return ae(e,t,t)}e.nextIndex=n.index+n[0].length}_(e,m(n[0]))},Ce),p((e,n)=>{let{autolink:o}=n.groups,t=i("a",m(o));t.href=o,_(e,t)},je),p((e,n)=>{let{email:o}=n.groups,t=i("a",m(o));t.href="mailto:"+o,_(e,t)},Oe),oe(Se),oe(Me),p((e,n)=>{let{tagstart:o,tagend:t,innerhtml:a}=n.groups;H(e,o+t),t==">"&&a&&(c(e,f(e).parent.lastElementChild,1),q(T(e,a)),x(e))},Xe)],P;function q(e){if(!/^\s*$/.test(e.input)){for(P=P||pe(le,!1);de(P,le,e,!0););he(e)}}function ie(e,n,o,t){let a=n[0],l=n.index+a.length>=e.input.length?"":"|\\s*$",s=e.blocks.length-1;if(e.blocks[s].element.tagName=="P"){let k=e.blocks[s-1];if(k.element.tagName!="LI"&&(l.length==0||t&&t!="1")){e.nextIndex=n.index;return}b(e),k.element.tagName=="LI"?re(e,k):x(e)}let d=a.length,h=o.length;h==0?d++:(h>4||l.length==0)&&(d-=h-1);let F=f(e),v=a.replaceAll(/\d+|[+*.)]|\s+$/g,k=>k[0]==" "?`(?:${k}|$)`:Number.isFinite(Number(k))?"\\d{1,9}":"\\"+k),U=new RegExp(`(?= {${d}}${l}|${v})`,"yui");if(t&&F.element.tagName!="OL"){let k=i("ol");t!="1"&&(k.start=Number.parseInt(t)),c(e,k,1,!1,void 0,U)}else!t&&F.element.tagName!="UL"&&c(e,i("ul"),1,!1,void 0,U);c(e,i("li"),1,!1,void 0,new RegExp(` {${d}}${l}`,"yui"),re),e.nextIndex=n.index+d}function re(e,n){let o=e.blocks.length-1;if(n.lines.length==0&&n==e.blocks[o-1]&&n.element.childElementCount==0&&e.blocks[o].element.tagName=="P"){let t=e.blocks.pop();_(e,...t.element.childNodes),n.lines=t.lines}}function Ge(e,n){me(e,n);let o=/ {0,3}(?:`|~){3,}\s*$/yiu;o.lastIndex=e.nextIndex,o.exec(e.input)&&(e.nextIndex=o.lastIndex)}function Je(e,n){let[o,t]=u.exec(e.input,e.nextIndex);!o||u.accepted?(D(e,n),e.nextIndex=t):e.nextIndex=e.input.length}function Ve(e,n){u.exec(e.input,e.nextIndex,!0),D(e,n)}function D(e,n){if(u.accepted){let{label:o,dest:t,title:a}=u.groups;if(o&&t){o=o.trim().toUpperCase(),t=z(t.trim()),a=z(a),e.linkRefs[o]||(/^<.*>$/.test(t)&&(t=t.slice(1,t.length-1)),e.linkRefs[o]={destination:t,title:a},e.links[o]?.forEach(l=>{t&&(l instanceof HTMLAnchorElement?l.href=t:l.src=t),a&&(l.title=a)}),delete e.links[o]),x(e);return}}n.element=i("p"),n.parent=n.element,n.type=1,n.cont=E}function se(e){B(e)||(b(e),c(e,i("p"),1,!0,void 0,E))}var _e=[p((e,n)=>{let{setext:o,setextspaces:t}=n.groups,a=f(e),l=n[0].trim().length;a.element.tagName=="P"?(a.element=i(o=="="?"h1":"h2"),a.parent=a.element,b(e),x(e)):o=="-"&&l>2?(b(e),_(e,i("hr"))):o=="-"&&l==1?ie(e,n,t):(c(e,i("p"),1,!0,void 0,E),_(e,m(n[0])))},/ {0,3}(?<setext>-|=)\k<setext>*(?<setextspaces>\s*)$/.source),p(e=>{A(e),_(e,i("hr"))},/ {0,3}(?<brkchar>[*\-_])(?:\s*\k<brkchar>){2,}\s*$/.source),p((e,n)=>{A(e);let{atxlevel:o,atxheader:t}=n.groups,a=o.length;c(e,i(`h${a}`),1),q(T(e,t)),x(e)},/ {0,3}(?<atxlevel>#{1,6})\s+(?<atxheader>.*?)\s*$/.source),p(e=>{if(!B(e)){b(e);let n=i("code");c(e,i("pre",n),0,!0,n,Be,me)}},Ae.source),p((e,n)=>{let{codefence:o,codelang:t}=n.groups;A(e);let a=new RegExp(`(?! {0,3}${o}+\\s*$)`,"yui"),l=i("code");t&&(l.className=`language-${z(t)}`),c(e,i("pre",l),0,!0,l,a,Ge)},/ {0,3}(?<codefence>(?:`|~){3,})(?:\s*(?<codelang>[^\s`~]+))?\s*$/.source),p(e=>{b(e);let n=/(?!.*(?:<\/pre>|<\/script>|<\/style>|<\/textarea>))/yui,o=e.input;n.lastIndex=e.nextIndex,n.test(o)?c(e,f(e).parent,2,!0,void 0,n,Fe):(H(e,o.slice(e.nextIndex)+`
`),e.nextIndex=o.length)},/(?= {0,3}<pre|<script|<style|<textarea)/.source),p(e=>{b(e),c(e,f(e).parent,2,!0,void 0,E)},/(?= {0,3}<[a-z][a-z0-9\-]*(?:\s+[a-z:_][\w.:-]*\s*(?:=\s*"[^"]*"|\s*='[^']*'|=[^\s"'=<>`]+)?)*\s*>\s*$)/.source),p(e=>{A(e),c(e,i("blockquote"),1,!1,void 0,Y)},Y.source),p((e,n)=>{let{bulletno:o,bulletsep:t}=n.groups;ie(e,n,t,o)},/ {0,3}(?:[\-+*]|(?<bulletno>\d{1,9})[.)])(?<bulletsep> +|$)/.source),p((e,n)=>{if(!B(e)){u.init();let[o]=u.exec(e.input,n.index);if(o){if(b(e),c(e,f(e).parent,3,!0,void 0,E,Ve,Je),u.accepted)return e.nextIndex=e.input.length,D(e,f(e))}else se(e)}e.nextIndex=n.index},u.startRegExp),p(se,E.source)],Ye=pe(_e,!0);function b(e){let n=f(e);if(n.lines.length>0){switch(n.type){case 1:q(T(e,n.lines.map(o=>o.trim()).join(`
`)));break;case 0:_(e,m(n.lines.join(`
`)));break;case 2:n.lines.push(""),H(e,n.lines.join(`
`));break}n.lines=[]}}function fe(e,n){for(let o=n;o<e.blocks.length;++o){let t=e.blocks[o];t.closing?.(e,t)}for(;e.blocks.length>n;)b(e),x(e)}function Ke(e){for(let n=0;n<e.blocks.length;++n){let o=e.blocks[n];if(o.cont){if(o.cont.lastIndex=e.nextIndex,!o.cont.exec(e.input))return fe(e,n);e.nextIndex=o.cont.lastIndex,o.continuing?.(e,o)}}}function ke(e,n){let o={input:"",nextIndex:0,blocks:[],linkRefs:{},links:{}};c(o,n,1,!1);let t=e.split(`
`);for(let a=0;a<t.length;++a){let l=t[a],s=T(o,l,0);Ke(s);let r=f(s);if(!r.leaf||r.element.tagName=="P")for(;de(Ye,_e,s,!1)&&(r=f(s),!r.leaf););r.lines.push(l.slice(s.nextIndex))}fe(o,0)}var en=Promise.resolve().then(()=>J(be())),N,j=0,M=0,ue=0,O=class extends Z.StyledElement{static get observedAttributes(){return["examples"]}constructor(){super("commonmark")}connect(){this.runExamples()}get examples(){let n=[],o=this.getAttribute("examples");if(o){let t=/\s*(?<sep>[,\-])?\s*(?<num>\d+)/g,a,l;for(;l=t.exec(o);){let{sep:s,num:r}=l.groups,d=Number.parseInt(r);if(a&&s=="-")for(let h=a+1;h<d;++h)n.push(h);n.push(d),a=d}}return n}async runExamples(){N||(N=await en),this.examples.forEach(n=>this.runTest(N[n-1]))}runTest(n){let o=0,t="<<Not run>>";try{let l=i("div"),s=window.performance.now();ke(n.markdown,l),o=window.performance.now()-s,t=l.innerHTML}catch(l){l instanceof Error&&(t=l.message+`
`+l.stack)}let a=t==n.html;a?j++:M++,ue+=o,this.renderTest(n,t,a,o)}renderTest(n,o,t,a){let l=i("th",m(t?"PASS":"FAIL"));l.className=t?"pass":"fail",this.body.append(i("table",i("tr",i("th",m(`Example: ${n.example}`)),i("th",m(n.section)),l),i("tr",i("th",m("Input")),i("th",m("Expected")),i("th",m("Actual"))),i("tr",i("td",i("pre",i("code",...I(n.markdown)))),i("td",i("pre",i("code",...I(n.html)))),i("td",i("pre",i("code",...I(o))))),i("tr",i("td",m(`In ${a.toFixed(1)}ms`)))))}},X=class extends Z.StyledElement{constructor(){super("commonmark")}connect(){window.requestIdleCallback(()=>this.render())}render(){let n=j+M;this.body.append(i("table",i("tr",i("th",m("Tests Run")),i("th",m("Succeeded")),i("th",m("Failed")),i("th",m("CommonMark Coverage"))),i("tr",i("td",m(n.toString())),i("td",m(j.toString())),i("td",m(M?M.toString():"-")),i("td",m((n*100/652).toFixed(1)+"%"))),i("tr",i("td",m(`In ${ue.toFixed(1)}ms`)))))}};customElements.define("commonmark-runner",O);customElements.define("commonmark-summary",X);})();
