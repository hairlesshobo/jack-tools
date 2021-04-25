(
 var h = "127.0.0.1";
 var p = 57140;
 var b = 512;
 var sz = 512;
 var m = "signal";
 var g = 1.0;
 var c = "grey";
 var s = "dot";
 var a = NetAddr(h,p);
 var nb = {arg n,l,h,d,s,m,a; NumberBox().value_(n).clipLo_(l).clipHi_(h).decimals_(d).step_(s).scroll_step_(m).action_(a)};
 var cb = {arg s, a; var v = CheckBox().string_(s).action_({a.(v.value)}); v};
 var te = {arg s, a; TextField().string_(s).action_(a)};
 var tx = {arg s; StaticText().string_(s)};
 var mk_cmd = {["rju-scope -U -n 2 -b",b,"-m",m,"-g",g,"-c",c,"-s",s,"-h",sz,"-w",sz,"-p SuperCollider:out_%d"].join(" ")};
 var lc_status = false;
 var lc = {arg x; if(x,{mk_cmd.().unixCmd},{"killall rju-scope".unixCmd}); lc_status =x};
 var js = cb.("local-scope",lc);
 var md = PopUpMenu().items_(["signal","embed","hline","hscan"]).action_({arg x; m = x.item; a.sendMsg('/mode',m)});
 var st = PopUpMenu().items_(["dot","line","fill"]).action_({arg x; s = x.item; a.sendMsg('/style',s)});
 var cl = PopUpMenu().items_(["grey","ega64"]).action_({arg x; c = x.item; a.sendMsg('/colour-mode',c)});
 var fr = nb.(512,16,2048,0,16,16,{arg x; a.sendMsg('/frames',x.value.asInt)});
 var gn = nb.(1,0.25,8,2,0.1,0.05,{arg x; g = x.value; a.sendMsg('/input-gain',g)});
 var dl = nb.(100,10,500,0,10,5,{arg x; a.sendMsg('/delay',x.value)});
 var in = nb.(1,0.05,2,2,0.1,0.05,{arg x; a.sendMsg('/incr',x.value)});
 var em = nb.(6,0,128,0,1,1,{arg x; a.sendMsg('/embed',x.value.asInt)});
 var v = [[tx.("host"),te.("127.0.0.1",{arg x; h = x; a = NetAddr(h,p)}),
           tx.("port"),nb.(57140,57140,57150,0,1,1,{arg x; p = x; a = NetAddr(h,p)})],
          [tx.("window-size"),nb.(512,128,1024,0,128,128,{arg x; sz = x.value.asInt}),
           tx.("frame-alloc"),nb.(512,256,2048,0,256,256,{arg x; b = x.value.asInt}),
           js],
          [tx.("mode"),md,tx.("style"),st,tx.("colour"),cl],
          [tx.("frames"),fr,tx.("gain"),gn,tx.("delay"),dl],
          [tx.("incr"),in,tx.("embed"),em]];
 var x0 = 200;
 var y0 = Window.screenBounds.height - 200;
 var w = Window("scope",bounds:Rect(x0,y0,720,160),resizable:false).layout_(GridLayout.rows(*v)).front;
 w.onClose_({if(lc_status,{"killall rju-scope".unixCmd})});
)
