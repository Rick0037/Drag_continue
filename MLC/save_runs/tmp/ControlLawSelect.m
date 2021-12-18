function [out1,out2,out3]=ControlLawSelect(s,t,n)
switch n
   otherwise
   out1=0;
   out2=0;
   out3=0;
end

end

function q=my_div(x,y)
protection = 1e-6;
y(y==0)=inf;
q=x./(y.*(abs(y)>protection)+protection*sign(y).*(abs(y)<=protection));
end

function q=my_log(x)
protection = 1e-6;
q=log10(abs(x).*(abs(x)>=protection)+protection*(abs(x)<protection));
end