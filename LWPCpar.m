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

arch=computer('arch');

lwpcProgram=sprintf('LWPC.%s',arch);

try
     worker=getCurrentTask;
     if ~isempty(worker)
          worker=worker.ID;
     end
catch
     worker=[];
end

folder=sprintf('lwpcpar%g',worker);
path=pwd;

if exist(sprintf('lwpcpar%g',worker),'dir')==0
    system(sprintf('cp -r lwpcpar %s',folder));
end

fid=fopen(sprintf('%s/lwpcDAT.loc',folder),'wt');
% fprintf(fid,'%s/%s/lwpcv21.%s/data/',path,folder,arch); 
fprintf(fid,'%s/%s/lwpcv21/data/',path,folder); 


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

%% Reset transmitter and model files

cd(sprintf('%s/%s',path,folder));

% system(sprintf('cp lwpcv21.%s/save/xmtr.lis lwpcv21.%s/data',arch,arch));
system(sprintf('cp lwpcv21/save/xmtr.lis lwpcv21/data',arch,arch));

system('rm lwpc.mds');

%% Run LWPC

system(sprintf('./%s lwpc',lwpcProgram));

% Lazy symbol binding: system(sprintf('./%s lwpc',lwpcProgram));

cd(path)

%% Read output log

a=textread(sprintf('%s/lwpc.log',folder),'%s','headerlines',34); %open outputed log file
power_lwpm=a{5};
dist_lwpm=a{4};

power_lwpm=str2double(power_lwpm);
dist_lwpm=str2double(dist_lwpm);

end
