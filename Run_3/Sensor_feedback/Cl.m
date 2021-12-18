function y=Cl(t)
global coefficent
T=round(10*(t-0.1))/10; % because the  
	
Time = coefficent(:,1);

y=coefficent(Time==T,6);
end 