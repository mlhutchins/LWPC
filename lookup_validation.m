% Lookup validation.m
% This is not a routine daily file. It can be used to validate the generated lookup tables
%	by checking for missing values, if missing values are found it jitters the stroke
%	locations until a valid solution is found.
%
% Written By:  Michael Hutchins

	if parallel_check
		parallel_start;
	end

	ticL=tic;

%% Load Stations
	stations

	removedStations = [2,4,9,13,14,23,27:32,34];

%% List of stations to process

	masterList = [0 : size(station_loc,1) - 1];
	
	masterList(removedStations) = [];
	
	masterList = reshape(masterList,4,length(masterList)/4);

	
%% Get current computer name (if dividing between flashes)

    [check,cName] = system('hostname');

    if ~isempty(strfind(cName,'flash5'))
        stationList = masterList(1,:);
    elseif ~isempty(strfind(cName,'flash6'))
        stationList = masterList(2,:);
    elseif ~isempty(strfind(cName,'flash7'))
        stationList = masterList(3,:);
    elseif ~isempty(strfind(cName,'flash8'))
        stationList = masterList(4,:);
	else
        fprintf('Wrong computer/hostname\n');
    end

%% Run through each station

for n = 1 : length(stationList)%stations

	i = stationList(n) + 1;
		
    fprintf('%s Station Started : %g seconds \n',station_name{i},toc(ticL));
    
    fileDay = sprintf('lookup_temp/lookup_day_temp_%02g.dat',i-1);
    fileNight = sprintf('lookup_temp/lookup_night_temp_%02g.dat',i-1);
    fileDist = sprintf('lookup_temp/lookup_dist_temp_%02g.dat',i-1);
    
    [day,res] = lookup_import(fileDay);
    [night,res] = lookup_import(fileNight);
    [dist,res] = lookup_import(fileDist);

    long = (1:res:360)-181+res/2;
    lat = (1:res:180)-91+res/2;
    
    day = squeeze(day(:,:,2));
    night = squeeze(night(:,:,2));
    dist = squeeze(dist(:,:,2));
    
    for j = 1 : length(long);
        for k = 1 : length(lat);
            
            %Check Day
			
            long1 = long(j);
            lat1 = lat(k);
            
            while isnan(day(j,k))

                update = zeros(11,1);
                
				parfor m = 1:11
					update(m) = LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],...
										station_loc(i,1),station_loc(i,2),'day');
				end
                    
				long1 = long1 + res/10;
				lat1 = lat1 + res/10;
				
                day(j,k) = nanmean(update);
                
            end
            
            %Check Night
 			
            long1 = long(j);
            lat1 = lat(k);
            
			while isnan(night(j,k))

				update = zeros(11,1);

				parfor m = 1:11
					update(m) = LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],...
										station_loc(i,1),station_loc(i,2),'night');
				end

				long1 = long1 + res/10;
				lat1 = lat1 + res/10;

				night(j,k) = nanmean(update);

			end
			
            %Check Dist
            if isnan(dist(j,k))
                
                dist(j,k) = vdist(lat1,long1,station_loc(i,1),station_loc(i,2))./1000;
              
            end            
            
        end
    end
    
    fprintf('%s Station Validation Finished : %g seconds \n',station_name{i},toc(ticL));

    lookup_day_single = day;
    lookup_night_single = night;
    lookup_dist = dist;

    fid=fopen(sprintf('lookup_day_temp_%02g_val.dat',i-1),'a+');
    fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
    for K=1:size(lookup_day_single,1);
        fprintf(fid,'%g\t',lookup_day_single(K,:));
        fprintf(fid,'\n');
    end
    fprintf(fid,'\n');
    fclose all;

    fid=fopen(sprintf('lookup_night_temp_%02g_val.dat',i-1),'a+');
    fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
    for K=1:size(lookup_night_single,1);
        fprintf(fid,'%g\t',lookup_night_single(K,:));
        fprintf(fid,'\n');
    end
    fprintf(fid,'\n');
    fclose all;

    fid=fopen(sprintf('lookup_dist_temp_%02g_val.dat',i-1),'a+');
    fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
    for K=1:size(lookup_dist,1);
        fprintf(fid,'%g\t',lookup_dist(K,:));
        fprintf(fid,'\n');
    end
    fprintf(fid,'\n');
    fclose all;

end

fprintf('Lookup Validation Complete : %g seconds \n',toc(ticL));

    

