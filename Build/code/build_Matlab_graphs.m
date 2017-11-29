speed = csvread("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/speed_Morocco.csv", 1, 0);
chars = csvread("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/characteristics/characteristics_Morocco.csv", 1, 0);

Morocco = graph(speed, 'upper')

edgeweights = 5*(Morocco.Edges.Weight / (max(Morocco.Edges.Weight)));
msize = 25*(chars(:,7) / max(chars(:,7)))+0.1;

plot(Morocco, 'XData', chars(:,3), 'YData', chars(:,4), 'Linewidth', edgeweights, 'MarkerSize', msize)
