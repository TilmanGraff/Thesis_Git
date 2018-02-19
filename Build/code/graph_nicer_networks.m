clear;
country_table = readtable("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/country_names.csv");

country_names = table2array(country_table);

centroids = readtable("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);


% centroids.productivity = cellfun(@str2double,centroids.lights) ./ cellfun(@str2double,centroids.pop);


for countryID = 1:length(country_names)
%for countryID = 3
    
    countryname = (country_names(countryID));

    speed = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_", (countryname), ".csv"), 1, 0);

    
    countryfile = centroids(centroids.country==countryname,:);

    if sum(sum(speed) ~= 0)
        
      I_opt = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Optimised_Networks/", (countryname), ".csv"), 0, 0);

      outcomes = readtable(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"), 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);

      welfare_gain = outcomes.util_opt ./ outcomes.util_stat;

      pop_dens = cellfun(@str2double, countryfile.pop_dens);
      second_biggest = max(pop_dens(pop_dens<max(pop_dens)));
        
      graphfile1= graph(speed, 'upper');
      graphfile2 = graph(I_opt, 'upper');
      
      edgeweights1 = 45*((graphfile1.Edges.Weight - 3.9) / (max(graphfile2.Edges.Weight)));
      msize = 17*((cellfun(@str2double, countryfile.pop_dens)) / second_biggest)+0.1;

      plot = plot(graphfile1, 'XData', (cellfun(@str2double, countryfile.x)), 'YData', (cellfun(@str2double, countryfile.y)), 'Linewidth', edgeweights1, 'MarkerSize', msize,  'NodeLabel', {});
      title(countryname)  
      print(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Matlab_graphs/Nicer_graphs/", (countryname), "_stat"), '-dpng');
      clear graphfile edgeweights msize plot
        
      

      edgeweights2 = 35*((graphfile2.Edges.Weight - 3.9) / (max(graphfile2.Edges.Weight)));
      msize = 17*((cellfun(@str2double, countryfile.pop_dens)) / second_biggest )+0.1;

      plot = plot(graphfile2, 'XData', (cellfun(@str2double, countryfile.x)), 'YData', (cellfun(@str2double, countryfile.y)), 'Linewidth', edgeweights2, 'MarkerSize', msize, 'NodeCData', welfare_gain, 'NodeLabel', {});
      title(countryname)
      colorbar

      countryname
      print(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Matlab_graphs/Nicer_graphs/", (countryname), "_opt"), '-dpng');
      clear graphfile edgeweights msize plot
        
    end

    


end
