function [power_lwpm,dist_lwpm]=LWPCpar(freq,lat,long,time,stat_lat,stat_long,model,power)
%% LWPCpar(freq,lat,long,time,stat_lat,stat_long,model)
%   Runs the LWPC code spread over multiple workers enabled with the parrallel computing toolbox
%   it cannot run without the toolbox.

%% Format Input

switch nargin
    case 6
        model='time';
        power=100;
    case 7
        power=100;
end

%% Get Worker ID and check for code

lwpcProgram=sprintf('LWPC.%s',computer('arch'));

worker=getCurrentTask;
worker=worker.ID;

folder=sprintf('lwpcpar%g',worker);
path=pwd;

if exist(sprintf('lwpcpar%g',worker),'dir')==0
    system(sprintf('cp -r lwpc %s',folder));
    fid=fopen(sprintf('%s/lwpcDAT.loc',folder),'wt');
    fprintf(fid,'%s/%s/lwpcv21/data/',path,folder); 
end

%% Write input file

% cd LWPC

fid=fopen(sprintf('%s/lwpc.inp',folder),'wt');
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
% fprintf(fid,'case-id %s\ntx %s\ntx-data lwpc %g %7.3f %7.3f 100 0 0 0\nionmatlabosphere homogeneous exponential 0.3 74\nreceivers %f %f\nlwflds\nprint-swg 0\nprint-mds 0\nprint-lwf 1\nlwf-vs-distance 20000 10000\nstart\nquit',text{1:7});
% fclose(fid);

%% Reset transmitter and model files

system(sprintf('cp %s/lwpcv21/save/xmtr.lis %s/lwpcv21/data',folder,folder));

% if isempty(strmatch('homog',model));
system(sprintf('rm %s/%s/lwpc.mds',path,folder));
% end
% system('del C:\LWPCv21\save\delete\lwpc_cal.lwf');

%% Run LWPC

%fprintf('%s/%s/LWPC lwpc\n',path,folder);

cd(sprintf('%s/%s',path,folder));

system(sprintf('%s/%s/%s lwpc',path,folder,lwpcProgram));

cd(path)

%eval(sprintf('cd %s',path));
%% Read output log

a=textread(sprintf('%s/lwpc.log',folder),'%s','headerlines',34); %open outputed log file
power_lwpm=a{5};
dist_lwpm=a{4};

power_lwpm=str2double(power_lwpm);
dist_lwpm=str2double(dist_lwpm);

% cd ..

% cd ~/Documents/MATLAB
end
