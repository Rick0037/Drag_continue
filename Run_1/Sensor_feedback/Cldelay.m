function y=Cldelay(t)
global coefficent
T=round(10*(t-2.3))/10;  
	
Time = coefficent(:,1);

y=coefficent(Time==T,7);
end 