while 1
    if exist('Evaluate_Barrier', 'file') == 2 
        system('rm -f Evaluate_Barrier');
        system('./UNS3');
    end
end
