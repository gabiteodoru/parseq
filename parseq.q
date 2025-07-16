f: asc `abs`cor`ej`gtime`like`mins`prev`scov`system`wavg`acos`cos`ema`hclose`lj`ljf`mmax`prior`sdev`tables`where`aj`aj0`count`enlist`hcount`load`mmin`rand`select`tan`while`ajf`ajf0`cov`eval`hdel`log`mmu`rank`set`til`within`all`cross`except`hopen`lower`mod`ratios`setenv`trim`wj`wj1`and`csv`exec`hsym`lsq`msum`raze`show`type`wsum`any`cut`exit`iasc`ltime`neg`read0`signum`uj`ujf`xasc`asc`delete`exp`idesc`ltrim`next`read1`sin`ungroup`xbar`asin`deltas`fby`if`mavg`not`reciprocal`sqrt`union`xcol`asof`desc`fills`ij`ijf`max`null`reval`ss`update`xcols`atan`dev`first`in`maxs`or`reverse`ssr`upper`xdesc`attr`differ`fkeys`insert`mcount`over`rload`string`upsert`xexp`avg`distinct`flip`inter`md5`parse`rotate`sublist`value`xgroup`avgs`div`floor`inv`mdev`peach`rsave`sum`var`xkey`bin`binr`do`get`key`med`pj`rtrim`sums`view`xlog`ceiling`dsave`getenv`keys`meta`prd`save`sv`views`xprev`cols`each`group`last`min`prds`scan`svar`vs`xrank`ww;
t:{@[{type parse string x};x;`$"Not a function"]} each f!f; 
x: ([] f:key t; h:{$[-5h=type x;x;0]}each value t);
x: update p: (parse string@) each f from x where h<>0;
revDict: (x`p)!string (x`f);
glyphs: asc "~`!@#$%^&*()-=+\":'/?.>,<";
types: string ``Bool`Guid``Byte`Short`Int`Long`Real`Float`Char`Symbol`Timestamp`Month`Date`Datetime`Timespan`Minute`Second`Time;
isOnlyGlyphs: {all {any x in glyphs} each x};
sstring: {$[type[x]=10h;x;string x]};
paren:{[s;p] p,s,("([{"!")]}")p};
parenl:{[l;p;sep] paren[sep sv l;p]};
ltrim2:{((x in " \n\r\t")?0b)_x};
rtrim2: {neg[((reverse x in " \n\r\t")?0b)]_x};
trim2: rtrim2 ltrim2 @;
extractBody:{[funcStr]
    body: trim2 1_-1_ trim2 funcStr;
    if["["=first body;
      body:trim2 (1+first ss[body;"]"])_body];
    body
    };
func2string:{t: type x; revX: revDict x; 
    $[
        isOnlyGlyphs sstring x; "Builtin",paren[sstring x;"["]; 
        count[revX]>0; "Builtin",paren[revX;"["];
        t=100h; "Lambda",paren[func100string x;"["];
        string x]};
func100string:{v: value x; parenl[string v[1]; "["; ", "], ", ", var2string parse extractBody last v};
/ var2string: {[x] t: type[x]; if[t>=100h; :"Func[",func2string[x],"]";]; 
/     if[t=99h; :"{",var2string[key x],": ",var2string[value x],"}}";];
/     at: abs t; $[t=0; "L","[",(", " sv var2string each x),"]"; t>0;"L",string[.Q.t t],"[",(", " sv sstring each x),"]";string[.Q.t neg t],"[",sstring[x],"]"]};

var2string: {[x] t: type[x]; if[t>=100h; :func2string[x];]; 
    if[t=99h; :"{",var2string[key x],": ",var2string[value x],"}";];
    at: abs t; $[t=0; "[",(", " sv var2string each x),"]"; t>0;"L",types[t],"[",(", " sv sstring each x),"]";types[neg t],"[",sstring[x],"]"]};

type first parse "a:x+1"
var2string parse "{(2#x)#1,x#0}"
var2string parse "{x+1}"
ff: {[expr] :var2string expr};
parse extractBody last v
expr
parse "`s`t!(min s; min t)"
parse "`s`t!((min;s); (min;t))"
parse "(min s; min t)"
parse "((min;s); (min;t))"
parse "select min s, maxs t from c"
parse "f[(min s; min t)]"
parse "f[x;`s`t!(min s; min t)]"
vp: var2string parse @;
vp "2+2"
var2string parse "f[(min s; min t)]"
var2string parse "`s`t!(min s; min t)"
var2string parse "`s`t!((min;s); (min;t))"
var2string parse "select min s, maxs t from c"
var2string parse "exec min s from c"
var2string parse "-1 \"asd\""
var2string parse "f[min sums c]"
var2string parse "f[min x;max y]"
var2string parse "select min s from c"
var2string  parse "{x+1}"
var2string  parse "f[`a;2]"
var2string  parse "{[x] ({(y+x%y)%2f}[x]/) x%2f}"
var2string  parse "f[\"asd\"]"
var2string  parse "f[1;2;]"
parse"(f/)x"
var2string parse"n f/x"
a:({x+y}\)
a[1;1 2 3]
a[1 2 3]
$[3>2;"asd";"fgh";"ghj"]
a[1 2 3]
2+2
parse "![a;b;c]"
![`a;`b;`c;]
$[3>2;"asd";]
@[1 2 3+;1;]`e
?[;"asd";"fgh"]101b

+/'{x*y}\[1 1;10#2]
lj

nmsq: {({0.5*y+x%y}[x]/)x%2};
nmroot: {[n;a] ({((y-1)*z + a%z xexp y-1)%y}[n;a]/)a%2f}
nmroot: {[x;n] ({[x;n;y] (((n-1)*y)+x%y xexp n-1)%n}[x;n]/)x%n};

nmroot[16;2]