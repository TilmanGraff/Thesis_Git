clear;
country_table = readtable("/Users/tilman/Documents/GitHub/Thesis_Git//Build/temp/country_names.csv");

country_names = table2array(country_table);

centroids = readtable("/Users/tilman/Documents/GitHub/Thesis_Git//Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);


centroids.productivity = cellfun(@str2double,centroids.lights) ./ cellfun(@str2double,centroids.pop);


for countryID = 1:length(country_names)


    countryname = (country_names(countryID));

    speed = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/temp/speed/speed_", (countryname), ".csv"), 1, 0);
    countryfile = centroids(centroids.country==countryname,:);

    if sum(sum(speed) ~= 0)

      graphfile = graph(speed, 'upper');

      edgeweights = 5*(graphfile.Edges.Weight / (max(graphfile.Edges.Weight)));
      msize = 25*((cellfun(@str2double, countryfile.pop_dens)) / max((cellfun(@str2double, countryfile.pop_dens))))+0.1;

      plot = plot(graphfile, 'XData', (cellfun(@str2double, countryfile.x)), 'YData', (cellfun(@str2double, countryfile.y)), 'Linewidth', edgeweights, 'MarkerSize', msize, 'NodeCData', (cellfun(@str2double, countryfile.productivity)));
      colorbar;
      title(countryname)

      print(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/output/Matlab_graphs/initial_infrastructure", (countryname), "_graph"), '-dpng');

    end

    clear graphfile edgeweights msize plot


end
