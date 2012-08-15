function [power_lwpm,dist_lwpm]=LWPC(freq,lat,long,time,stat_lat,stat_long,model,power)

switch nargin
    case 6
        mode='time';
        power=100;
    case 7
        power=100;
end

% LWPC(freq,lat,long,time,stat_lat,stat_long,model)

cd LWPC

fid=fopen('lwpc.inp','wt');
%text goes: case-id, file name, lat, long, month, day year, hour, minute,
%receiver lat, receiver long

if not(isempty(strmatch('time',model)));
    
    %LWPM Model with Time
    text={'lwpc','lwpc',freq,lat,-long,power,month(time(2)),time(3),time(1),time(4),time(5),stat_lat,-stat_long};
    fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f %g 0 0 0\nionosphere lwpm %s/%g/%g %02g:%02g\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{:});
    fclose(fid);

elseif not(isempty(strmatch('day',model)));
    
    %LWPM Day model
    text={'lwpc','lwpc',freq,lat,-long,power,stat_lat,-stat_long};
    fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f %g 0 0 0\nionosphere lwpm day\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{:});
    fclose(fid);
    
elseif not(isempty(strmatch('night',model)));
    
    %LWPM Night model
    text={'lwpc','lwpc',freq,lat,-long,power,stat_lat,-stat_long};
    fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f %g 0 0 0\nionosphere lwpm night\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{:});
    fclose(fid);
    
elseif not(isempty(strmatch('homog',model)));
    
    %Homogeneous Exponential model
    text={'lwpc','lwpc',freq,lat,-long,power,stat_lat,-stat_long};
    fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f %g 0 0 0\nionosphere homogeneous exponential 0.3 74\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{:});
    fclose(fid);
    
else
    
    
    
    text={'lwpc','lwpc',freq,lat,-long,power,stat_lat,-stat_long};
    fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f %g 0 0 0\nionosphere lwpm day\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{:});
    fclose(fid);

end

% LWPM Homogeneous day
% text={'test2','test2',freq,lat,-long,stat_lat,-stat_long};
% fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f 100 0 0 0\nionosphere homogeneous exponential 0.3 74\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{1:7});
% fclose(fid);

system('cp LWPCv21/save/xmtr.lis LWPCv21/data');

% if isempty(strmatch('homog',model));
system('rm lwpc.mds');
% end
% system('del C:\LWPCv21\save\delete\lwpc_cal.lwf');

system('./lwpc lwpc');

a=textread('lwpc.log','%s','headerlines',34); %open outputed log file
power_lwpm=a{5};
dist_lwpm=a{4};

power_lwpm=str2double(power_lwpm);
dist_lwpm=str2double(dist_lwpm);

cd ..

% cd ~/Documents/MATLAB
end