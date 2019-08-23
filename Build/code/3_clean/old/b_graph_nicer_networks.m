clear;
country_table = readtable("/Users/tilman/Documents/GitHub/Thesis_Git//Build/temp/country_names.csv");

country_names = table2array(country_table);

centroids = readtable("/Users/tilman/Documents/GitHub/Thesis_Git//Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);


% centroids.productivity = cellfun(@str2double,centroids.lights) ./ cellfun(@str2double,centroids.pop);


for countryID = 1:length(country_names)
%for countryID = 3

    countryname = (country_names(countryID));
if exist(strcat("//Users/tilman/Documents/GitHub/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"))
    speed = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/temp/speed/speed_", (countryname), ".csv"), 1, 0);


    countryfile = centroids(centroids.country==countryname,:);

    if sum(sum(speed) ~= 0)

      I_opt = csvread(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/output/Optimised_Networks/", (countryname), ".csv"), 0, 0);

      outcomes = readtable(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/output/Network_outcomes/", (countryname), "_outcomes.csv"), 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);

      welfare_gain = outcomes.util_opt ./ outcomes.util_stat;

      pop_dens = cellfun(@str2double, countryfile.pop_dens);
      second_biggest = max(pop_dens(pop_dens<max(pop_dens)));

      graphfile1= graph(speed, 'upper');
      graphfile2 = graph(I_opt, 'upper');

      %edgeweights1 = 30*((graphfile1.Edges.Weight - 3.9) / (max(graphfile2.Edges.Weight)));
      edgeweights1 = sqrt(graphfile1.Edges.Weight) - sqrt(3.9);
      msize = 17*((cellfun(@str2double, countryfile.pop_dens)) / second_biggest)+0.1;

      plot = plot(graphfile1, 'XData', (cellfun(@str2double, countryfile.x)), 'YData', (cellfun(@str2double, countryfile.y)), 'Linewidth', edgeweights1, 'MarkerSize', msize,  'NodeLabel', {}, 'EdgeColor', [0.13 0.43 0.95]);
      title(countryname)
      print(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/output/Matlab_graphs/Nicer_graphs/", (countryname), "_stat"), '-dpng');
      clear graphfile edgeweights msize plot



      %edgeweights2 = 30*((graphfile2.Edges.Weight - 3.9) / (max(graphfile2.Edges.Weight)));
      edgeweights2 = sqrt(graphfile2.Edges.Weight) - sqrt(3.9)
      msize = 17*((cellfun(@str2double, countryfile.pop_dens)) / second_biggest )+0.1;

      plot = plot(graphfile2, 'XData', (cellfun(@str2double, countryfile.x)), 'YData', (cellfun(@str2double, countryfile.y)), 'Linewidth', edgeweights2, 'MarkerSize', msize, 'NodeCData', welfare_gain, 'NodeLabel', {}, 'EdgeColor', [0.13 0.43 0.95]);
      title(countryname)
      colorbar

      countryname
      print(strcat("/Users/tilman/Documents/GitHub/Thesis_Git//Build/output/Matlab_graphs/Nicer_graphs/", (countryname), "_opt"), '-dpng');
      clear graphfile edgeweights msize plot

    end

end


end
