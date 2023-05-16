function [ev,evl] = SetEpochEvents(FilesnameIn)
ev={};
evl={};

if FilesnameIn(10)== 'a'
	if FilesnameIn(16)== 'i'
		disp('audioSimple')
        ev = {'31','41'};  
        evl = {'32','42'};  
	else 
		disp('audiocomplexe')
		ev = {'11','21'}; 
        evl = {'12','22'};  
	end
 elseif FilesnameIn(10)== 'v'
	if FilesnameIn(17)== 'i'
		disp('visuelSimple')
		ev = {'15','25'};  
        evl = {'16','26'}; 
	else 
		disp('visuelcomplexe')
		ev = {'13','23'};
        evl = {'14','24'}; 
	end
 elseif FilesnameIn(10)== 't'
		disp('tactSimple')
		ev = {'1','2'}; 
        evl = {'17','18'}; 
end


end