%%lwpc_generate Regenerate LWPC tables for given stations

stations

ticL=tic;

removed_stations = [2,4,9,13,14,23,27:32,34];

matlabpool 4

parfor i=1:4%size(station_loc,1) %stations
    % for i=size(lookup_test,3)+3:size(station_loc,1)

    fprintf('%s Station Started : %g seconds \n',station_name{i},toc(ticL));

    div=90;
    long=(1:div:360)-181+div/2;
    lat=(1:div:180)-91+div/2;
    
    lookup_day = zeros(length(long),length(lat),11);
    lookup_night = lookup_day;
    lookup_dist = squeeze(lookup_day(:,:,1));
    
    if sum(i==removed_stations)==0

        for m=1:11
            for j=1:length(long);
                for k=1:length(lat);
                    long1=long(j);
                    lat1=lat(k);
                    if m==1;
                    [lookup_day(j,k,m),lookup_dist(j,k)]=LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'day');
                    lookup_night(j,k,m)=LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'night');
                    else
                    lookup_day(j,k,m)=LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'day');
                    lookup_night(j,k,m)=LWPCpar(m+7,lat1,long1,[2000,01,01,00,00],station_loc(i,1),station_loc(i,2),'night');
                    end
                end
            end
            fprintf('%s - Frequency %g kHz Done : %g seconds \n',datestr(now),m+7,toc(ticL));
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
    
    fid=fopen(sprintf('lookup_night_temp_%02g.dat',i-1),'a+');
    fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
    for K=1:size(lookup_night_single,1);
        fprintf(fid,'%g\t',lookup_night_single(K,:));
        fprintf(fid,'\n');
    end
    fprintf(fid,'\n');
    
    fid=fopen(sprintf('lookup_dist_temp_%02g.dat',i-1),'a+');
    fprintf(fid,sprintf('%s - %s\n',station_name{i},datestr(now)));
    for K=1:size(lookup_dist,1);
        fprintf(fid,'%g\t',lookup_dist(K,:));
        fprintf(fid,'\n');
    end
    fprintf(fid,'\n');
    
end

save lookup_generate


fprintf('Lookup Update Complete : %g seconds \n',toc(ticL));

    

