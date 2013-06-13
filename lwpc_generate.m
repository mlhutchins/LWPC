% lwpc_generate generates LWPC tables at a given resolution
%
%   Written by: Michael Hutchins

%% Start pools

    if parallel_check
        parallel_start;
    end

    ticL=tic;
    
%% Load current station data and list inactive stations

    stations

    removed_stations = [2,4,9,13,14,23,27:32,34];

%% Get current computer name (if dividing between flashes)

    [check,cName] = system('hostname');

    if ~isempty(strfind(cName,'flash7'))
        offset = 0;
    elseif ~isempty(strfind(cName,'flash8'))
        offset = 1;
    else
        fprintf('Wrong computer/hostname\n');
    end

%% Initialize cell arrays

    lookupDay=cell(size(station_loc,1),1);
    lookupNight=lookupDay;
    lookupDist=lookupDay;

%% Go through each station to generate tables

    parfor i= 1:size(station_loc,1) %stations

        fprintf('%s Station Started : %g seconds \n',station_name{i},toc(ticL));

        div=1;
        long=(1:div:360)-181+div/2;
        lat=(1:div:180)-91+div/2;

        lookup_day = zeros(length(long),length(lat),11);
        lookup_night = lookup_day;
        lookup_dist = squeeze(lookup_day(:,:,1));

        if ~ismember(i,removed_stations)

            for m=1:11
                for j=1:length(long);
                    for k=1:length(lat);
                        long1=long(j);
                        lat1=lat(k);
                        if m==1;
                        [lookup_day(j,k,m),lookup_dist(j,k)]=LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'day');
                        lookup_night(j,k,m)=LWPCpar2(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'night');
                        else
                        lookup_day(j,k,m)=LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'day');
                        lookup_night(j,k,m)=LWPCpar2(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'night');
                        end
                    end
                end
                fprintf('%s - %s - Frequency %g kHz Done : %g seconds \n',datestr(now),station_name{i},m+7,toc(ticL));
            end

        end

        lookupDay{i}=lookup_day;
        lookupNight{i}=lookup_night;
        lookupDist{i}=lookup_dist;
        lookupName{i}=station_name{i};

        fprintf('%s Station Finished : %g seconds \n',station_name{i},toc(ticL));

        lookup_day_single=squeeze(mean(lookup_day,3));
        lookup_night_single=squeeze(mean(lookup_night,3));

        fid=fopen(sprintf('lookup_day_temp_%02g.dat',i-1),'a+');
        fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
        for K=1:size(lookup_day_single,1);
            fprintf(fid,'%g\t',lookup_day_single(K,:));
            fprintf(fid,'\n');
        end
        fprintf(fid,'\n');
        fclose all;

        fid=fopen(sprintf('lookup_night_temp_%02g.dat',i-1),'a+');
        fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
        for K=1:size(lookup_night_single,1);
            fprintf(fid,'%g\t',lookup_night_single(K,:));
            fprintf(fid,'\n');
        end
        fprintf(fid,'\n');
        fclose all;

        fid=fopen(sprintf('lookup_dist_temp_%02g.dat',i-1),'a+');
        fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
        for K=1:size(lookup_dist,1);
            fprintf(fid,'%g\t',lookup_dist(K,:));
            fprintf(fid,'\n');
        end
        fprintf(fid,'\n');
        fclose all;

    end

%% Save .mat file as backup

    save lookup_generate

    fprintf('Lookup Update Complete : %g seconds \n',toc(ticL));