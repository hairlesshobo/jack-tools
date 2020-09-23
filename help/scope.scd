(
 var h = "127.0.0.1";
 var p = 57140;
 var a = NetAddr(h,p);
 var nb = {arg n,l,h,d,s,m,a; NumberBox().value_(n).clipLo_(l).clipHi_(h).decimals_(d).step_(s).scroll_step_(m).action_(a)};
 var tx = {arg s; StaticText().string_(s)};
 var md = PopUpMenu().items_(["signal","embed","hline","hscan"]).action_({arg x; a.sendMsg('/mode',x.item)});
 var st = PopUpMenu().items_(["fill","line","dot"]).action_({arg x; a.sendMsg('/style',x.item)});
 var cl = PopUpMenu().items_(["grey","ega64"]).action_({arg x; a.sendMsg('/colour-mode',x.item)});
 var fr = nb.(512,16,512,0,16,16,{arg x; a.sendMsg('/frames',x.value.asInt)});
 var gn = nb.(1,0.25,8,2,0.1,0.05,{arg x; a.sendMsg('/input-gain',x.value)});
 var dl = nb.(100,10,500,0,10,5,{arg x; a.sendMsg('/delay',x.value)});
 var in = nb.(1,0.05,2,2,0.1,0.05,{arg x; a.sendMsg('/incr',x.value)});
 var em = nb.(6,0,128,0,1,1,{arg x; a.sendMsg('/embed',x.value.asInt)});
 var v = [[tx.("mode"),md,tx.("style"),st,tx.("colour"),cl],
          [tx.("frames"),fr,tx.("gain"),gn,tx.("delay"),dl],
          [tx.("incr"),in,tx.("embed"),em]];
 var x0 = 200;
 var y0 = Window.screenBounds.height - 200;
 var w = Window("scope",bounds:Rect(x0,y0,500,120),resizable:false).layout_(GridLayout.rows(*v)).front;
)
